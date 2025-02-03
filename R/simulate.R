#' Simulate an SIR model with specified time-varying reproduction number
#'
#' Uses the
#'
#' @param S0 Initial number of susceptible individuals
#' @param I0 Initial number of infected individuals
#' @param R0 Initial number of recovered individuals
#' @param Rt A vector of time-varying reproduction number values. Default is a
#'  modified sine curve, oscillating between 0.50 and 1.50 with a period of 20 days.
#' @param gamma Recovery rate, the inverse of the average infectious period
#' @param delay_pmf Probability mass function for delay distribution.
#'   Default is a vector with mean 3.
#' @param k Dispersion parameter for negative binomial distribution
#' @param seed Random seed for reproducibility
#' @param date_0 Start date for the simulation. Only used to add dates to outputs for use
#'   with `RtGam()` and friends.
#' @param day_of_week Logical flag for day of the week effect (not used)
#' @return A list containing the time series of S, I, R, beta, true cases,
#'   true Rt, observed cases, and reference dates
#' @export
simulate_sir <- function(
    S0 = 99000,
    I0 = 1000,
    R0 = 0,
    Rt = 0.5 * sin(seq_len(100) / 10 * pi) + 1,
    gamma = 0.5,
    delay_pmf = c(0, 0, 0.1, 0.1, 0.3, 0.2, 0.1, 0.1, 0.1),
    k = 10,
    seed = 12345,
    date_0 = as.Date("2023-01-01"),
    day_of_week = FALSE) {
  S <- integer(length(Rt))
  I <- integer(length(Rt))
  R <- integer(length(Rt))
  beta <- double(length(Rt))
  incidence <- integer(length(Rt))

  S[1] <- S0
  I[1] <- I0
  R[1] <- R0
  N <- S0 + I0 + R0

  for (t in 1:(length(beta) - 1)) {
    # Catch case where S is zero causes beta to be NA
    if (S[t] == 0) {
      beta[t] <- 0
    } else {
      beta[t] <- Rt[t] * gamma * N / S[t]
    }
    dSdt <- -max(as.integer(beta[t] * S[t] * I[t] / N), 0L)
    dRdt <- max(as.integer(I[t] * gamma), 0)
    dIdt <- -dSdt - dRdt

    S[t + 1] <- S[t] + dSdt
    I[t + 1] <- I[t] + dIdt
    R[t + 1] <- R[t] + dRdt

    current_pop <- S[t + 1] + I[t + 1] + R[t + 1]
    if (current_pop != N) {
      cli::cli_abort(c(
        "Population is not conserved",
        "*" = "S = {S[t + 1]}",
        "*" = "I = {I[t + 1]}",
        "*" = "R = {R[t + 1]}",
        "*" = "total = {current_pop}",
        "*" = "t = {t}"
      ))
    }

    # Incidence is the number of new cases, I is the prevalence
    incidence[t + 1] <- -dSdt
  }

  true_cases <- convolve(incidence, rev(delay_pmf), type = "open")
  obs_cases <- rnbinom(
    n = length(Rt),
    mu = true_cases,
    size = k
  )

  return(list(
    S = S,
    I = I,
    R = R,
    incidence = incidence,
    beta = beta,
    true_cases = true_cases,
    true_rt = Rt,
    obs_cases = obs_cases,
    reference_date = date_0 + 0:(length(Rt) - 1)
  ))
}
