pkgs <- c("glmmTMB", "broom.mixed", "RTMB")
for (p in pkgs) {
  if (!require(p, character.only = TRUE)) stop(sprintf("need %s package installed", p))
}

## machinery from simulate_sir
#' @param x numeric vector
#' @param delay_pmf probability mass function of the delay distribution
do_convolve <- function(x, delay_pmf) {
  x_full <- stats::convolve(x, rev(delay_pmf), type = "open")
  x_full[length(delay_pmf):length(x_full)]
}

## brute-force convolution (FFT-based solution makes RTMB unhappy ...)
## AFAICT TMB does not have a native/atomic 'filter' or 'convolution' f'n
# params same as above
do_convolve2 <- function(x, delay_pmf) {
  d <- rev(delay_pmf)
  x_res <- d[1]*x
  for (j in 2:length(delay_pmf)) {
    x_res <- x_res + d[j]*c(x[-(1:(j-1))], rep(0, j-1))
  }
  x_res
}

nz_elements <- function(x) {
  x[lengths(x) > 0 ]
}

#' @param pars list of parameters (beta, b, theta)
#' data.tmb should contain: X, Z, delay_pmf, yobs, terms
nll_convolve <- function(pars) {
  getAll(pars, data.tmb)
  ## drop(as.matrix(...)) -- need to convert back from Matrix object
  eta <- drop(as.matrix(X %*% beta + Z %*% b))
  ## could use logspace addition but probably overkill
  ## eta, mu = report cases
  ## etac, muc = delay-convolved cases
  mu <- exp(eta)
  muc <- do_convolve2(mu, delay_pmf)
  etac <- log(muc)
  REPORT(mu)
  ADREPORT(eta)
  ADREPORT(etac)
  ## use dnbinom_robust (log-parameter version of dnbinom from TMB):
  ## https://kaskr.github.io/adcomp/group__R__style__distribution.html# \
  ## gaa23e3ede4669d941b0b54314ed42a75c
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

fit_RTMB_convolve <- function(form, family, data, REML = TRUE,
                              start = list(),
                              delay_pmf) {
  ## FIXME: if all theta parameters will be covariances we could just start them all at 3
  ##   instead of 0 by default ...
  if (length(start)==0)
    warning("default starting values may be unreliable. Increase log-SD (theta) start values?")
  ## FIXME: repeat less without being arcane? (do.call, or replacing head of call?
  m <- glmmTMB::glmmTMB(form = form, family = family, data = data,
                        REML = REML, start = start, doFit = FALSE)
  data.tmb <- c(m$data.tmb, list(delay_pmf = delay_pmf))
  pars0 <- nz_elements(m$parameters)
  assign("data.tmb", data.tmb, environment(nll_convolve))
  ff <- MakeADFun(nll_convolve, pars0, random = "b", silent = TRUE)
  fit <- with(ff, nlminb(par, fn, gr))
  class(ff) <- "TMB"  ## for S3 methods
  res <- list(fit = fit, obj = ff, data = data.tmb, start = pars0)
  class(res) <- "convolve_fit"
  return(res)
}

get_num <- function(x) {
  n <- stringr::str_extract(x, "[0-9]+$")
  num <- ifelse(is.na(n), 0, as.numeric(n))
}

augment.convolve_fit <- function(x, conf.int = TRUE, conf.level = 0.95, ...) {
  qq <- qnorm((1+conf.level)/2)
  RTMB::sdreport(x$obj) |>
    summary() |>
    as.data.frame() |>
    tibble::rownames_to_column("parameter") |>
    as_tibble() |>
    filter(grepl("^eta", parameter)) |>
    ## tidyr::separate_wider_delim is too awkward here
    transmute(
      rdate = get_num(parameter),
      ## extract and rename (because we're going to exponentiate below)
      parameter = gsub("\\.[0-9]+", "", parameter) |>
        gsub(pattern = "eta", replacement = "mu"),
      value = Estimate,
      lwr = Estimate - qq*`Std. Error`,
      upr = Estimate + qq*`Std. Error`) |>
    mutate(across(c(value, lwr, upr), exp))
}

#' @param delay_pmf prob mass function for delays (sum == 1.0)
#' @param nt number of time steps
#' @param theta_start starting value for log-SD of spline variance
#' @param seed RNG seed
#' @param k df for spline
simfit <- function(delay_pmf , nt = 100, theta_start = 3, seed = 101, k = 20) {
  Rt <- 0.5 * sin(seq_len(nt)/10 * pi) + 1  ## default Rt
  sim0 <- simulate_sir(Rt = Rt, delay_pmf = delay_pmf, seed = seed) |>
    dplyr::mutate(rdate = as.numeric(reference_date - min(reference_date)))
  ## sim0 has parameter in {"S","I","R","true_rt","incident_infections","true_incident_cases",
  ## "observed_incident_cases"
  ##
  ## get observed incident cases
  dd0 <- sim0 |>
    dplyr::filter(parameter == "observed_incident_cases") |>
    dplyr::select(date = reference_date, rdate, value)
  ## fancy evaluation to evaluate k here
  ## (need to coerce it *back* into a formula ...)
  form <- as.formula(bquote(value ~ s(rdate, k = .(k))))
  m <- fit_RTMB_convolve(form,
                         family = nbinom2, data = dd0,
                         start = list(theta = theta_start),
                         delay_pmf = delay_pmf)

  ## careful with matching up rdates (reference dates)
  ## augment() assumes starting with zero
  aa <- augment(m) |>
    dplyr::full_join(x = distinct(select(sim0, rdate)), by = "rdate")
  
  gam_fit <- gam(value ~ s(rdate, k = k), family = "nb", data = dd0, method = "REML",
                 na.action = na.exclude) ## na.exclude includes initial rdates
  gam_pred0 <- predict(gam_fit, type = "link", se.fit = TRUE)
  gam_pred <- with(gam_pred0, tibble(rdate = dd0$rdate,
                                     parameter = "gam_fit_unshifted",
                                     value = exp(c(fit)),
                                     lwr = exp(c(fit)-1.96*c(se.fit)),
                                     upr = exp(c(fit)+1.96*c(se.fit))))
  sum_pmf <- sum(delay_pmf)
  if (!isTRUE(all.equal(sum_pmf, 1.0))) warning("sum of pmf != 1 (%1.2g)", sum_pmf)
  mean_delay <- sum(seq_along(delay_pmf)*delay_pmf)
  gam_pred_shifted <- gam_pred |>
    mutate(parameter = "gam_fit_shifted",
           across(c(value, lwr, upr), ~ dplyr::lead(., round(mean_delay), default = NA)))
  ## do we want incident_infections?
  aa <- dplyr::bind_rows(filter(sim0,
                                parameter %in%
                                c("incident_infections", "observed_incident_cases", "true_incident_cases")),
                         ## double-check augment(), why do we have 10 NA rows at the end?
                         na.omit(aa),
                         gam_pred,
                         gam_pred_shifted)
  class(aa) <- c("RTMB_simfit", class(aa))
  return(aa)
}

delay_cv <- function(pmf) {
  inds <- seq_along(pmf)
  sqrt(sum(inds^2*pmf))/sum(inds*pmf)
}
