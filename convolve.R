## https://github.com/CDCgov/cfa-gam-rt/issues/88
library(RtGam)
library(dplyr)
library(ggplot2); theme_set(theme_bw())
library(mgcv)
library(glmmTMB)
library(RTMB)

## use default from simulate_sir() for now
delay_pmf <- (1:4)/10
delay_mean <- sum(seq_along(delay_pmf)*delay_pmf)

## machinery from simulate_sir
do_convolve <- function(x, delay_pmf) {
  x_full <- stats::convolve(x, rev(delay_pmf), type = "open")
  x_full[length(delay_pmf):length(x_full)]
}

## brute-force convolution (FFT-based solution makes RTMB unhappy ...)
## AFAICT TMB does not have a native/atomic 'filter' or 'convolution' f'n
do_convolve2 <- function(x, delay_pmf) {
  d <- rev(delay_pmf)
  x_res <- d[1]*x
  for (j in 2:length(delay_pmf)) {
    x_res <- x_res + d[j]*c(x[-(1:(j-1))], rep(0, j-1))
  }
  x_res
}

test <- ((1:10)-5)^2
stopifnot(all.equal(do_convolve(test, delay_pmf),
                    do_convolve2(test, delay_pmf)))

nz_elements <- function(x) {
  x[lengths(x) > 0 ]
}

set.seed(101)
## simulate with neg bin etc (accept defaults)
sim0 <- simulate_sir() |>
  mutate(rdate = as.numeric(reference_date - min(reference_date)))

## will want to compare true_rt, true_incident_cases
## also, maybe, true growth rate? (maybe adjust code to
## return true_Rt and true_rt/true_growth? Back-calculating
## r from R seems mildly tedious

dd <- sim0 |>
  filter(parameter == "observed_incident_cases") |>
  select(date = reference_date, rdate, value)


plot(value ~ rdate, dd)

## regular fit (ignore delays)
## using REML because most (?) reliable for estimating degree of smoothing
## bump k up from default
m1 <- gam(value ~ s(rdate, k = 20), family = "nb", data = dd, method = "REML")
plot(m1)

## confirming that glmmTMB can fit the same (unshifted/unconvolved) model
m2 <- glmmTMB(value ~ s(rdate, k = 20), family = nbinom2, data = dd,
              REML = TRUE)
m2$obj$env$parList()$theta

## have to start glmmTMB with a larger-than-default SD for the spline
## term ... otherwise collapses (could check profile etc etc)
m2B <- update(m2, start = list(theta = 3))
plot(predict(m2B))
lines(predict(m1), col = 2)
## predict.gam returns an odd named structure
stopifnot(all.equal(unname(c(predict(m1))), predict(m2B), tolerance = 1e-6))

## now use the glmmTMB model structure to fit a model with convolution instead

## create TMB-data object
## (glmmTMB handles calls to smoothCon, smooth2random(sm, vnames = "", type = 2);
##  might eventually want to disentangle/extract that code
m2C <- update(m2B, doFit = FALSE)

data.tmb <- c(m2C$data.tmb, list(delay_pmf = delay_pmf))
fn <- function(pars) {
  getAll(pars, data.tmb)
  ## drop(as.matrix(...)) -- need to convert back from Matrix object
  eta <- drop(as.matrix(X %*% beta + Z %*% b))
  ## could use logspace addition but probably overkill
  ## FIXME: do we need to log/exp as many times as we do?
  etac <- log(do_convolve2(exp(eta), delay_pmf))
  ## exponentiate & rename for report:
  mu <- exp(etac)
  REPORT(mu)
  ADREPORT(etac)
  ## use dnbinom_robust (log-parameter version of dnbinom from TMB):
  ## https://kaskr.github.io/adcomp/group__R__style__distribution.html#gaa23e3ede4669d941b0b54314ed42a75c
  ## copied from glmmTMB: parameters are log(mu), log(var-mu)
  ## betadisp = log(phi).  var = mu + mu^2/phi -> var - mu = mu^2*phi
  ## -> log(var-mu) = 2*log(mu) - log(phi)
  nll0 <- -sum(dnbinom_robust(yobs, etac, 2*etac - betadisp, log = TRUE))
  ## use 'terms' (list with blockReps, blockSize)
  ##   all spline terms will be homdiag (1 theta value)
  b_ind <- 0
  for (i in seq_along(terms)) {
    np <- terms[[i]]$blockSize
    nll0 <- nll0 - sum(dnorm(b[b_ind + seq_len(np)], 0, exp(theta[i]), log = TRUE))
  }
  return(nll0)
}

pars0 <- nz_elements(m2C$parameters)

## base-R, AD versions are identical if no random effects/Laplace approx:
fn(pars0)

ff <- MakeADFun(fn, pars0)
ff$fn()

ff2 <- MakeADFun(fn, pars0, random = "b", silent = TRUE)
ff2$fn()

fit <- with(ff2, nlminb(par, fn, gr))
plot(ff2$report()$mu)

## get predictions and CIs

obs_tvec <- unique(dd$rdate)
pp <- predict(m1, se.fit = TRUE)
mgcv_res <- tibble(parameter = "mgcv",
                   rdate = obs_tvec-delay_mean,
                   value = exp(pp$fit),
                   lwr = exp(pp$fit - 1.96*pp$se.fit),
                   upr = exp(pp$fit + 1.96*pp$se.fit))

RTMB_res <- RTMB::sdreport(ff2) |>
  summary() |>
  as.data.frame() |>
  tibble::rownames_to_column("param") |>
  filter(grepl("^etac", param)) |>
  transmute(
    parameter = "RTMB",
    rdate = 0:99,
    value = Estimate,
    lwr = Estimate - 1.96*`Std. Error`,
    upr = Estimate + 1.96*`Std. Error`) |>
  mutate(across(c(value, lwr, upr), exp))



res1 <- sim0 |>
  filter(parameter == "incident_infections") |>
  bind_rows(RTMB_res, mgcv_res)


## in this case mean-shifting and convolution do about equally well
ggplot(res1, aes(rdate, value)) + geom_line(aes(colour = parameter)) +
  geom_ribbon(aes(fill = parameter, ymin = lwr, ymax = upr),
              colour = NA, alpha = 0.2) +
  labs(x = "date", y = "incidence")
## (warning from NA values in true infections)

## TODO:
## * work out how to do stuff with factor/by smooths
##   (whatever RtGam is currently using ...) -- see what breaks
##   (should be transferrable back to glmmTMB ...)
## * will need extra machinery for prediction
## computational cost?
## fragility?

## end effects?
