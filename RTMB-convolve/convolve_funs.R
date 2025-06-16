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

fit_RTMB_convolve <- function(form, family, data, REML = TRUE,
                              start = list(),
                              delay_pmf) {
  ## FIXME: if all theta parameters will be covariances we could just start them all at 3 instead of 0 by default ...
  if (length(start)==0) warning("start parameter values may be unreliable. Consider bumping up log-SD (theta) start values")
  ## FIXME: repeat less without being arcane? (do.call, or replacing head of call?
  m <- glmmTMB::glmmTMB(form = form, family = family, data = data, REML = REML, start = start, doFit = FALSE)
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


  
