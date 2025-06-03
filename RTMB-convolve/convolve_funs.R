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

nz_elements <- function(x) {
  x[lengths(x) > 0 ]
}

nll_convolve <- function(pars) {
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
