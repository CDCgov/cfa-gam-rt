#' Simulate an SIR model with specified time-varying reproduction number
#'
#' Simulates the spread of an infectious disease using the
#' Susceptible-Infectious-Recovered (SIR) compartment model. The simulator takes a
#' vector of specified \eqn{R_t} values and uses them to calculate the transmissibility
#' of the disease at each time point. The model is then used to simulate the number of
#' susceptible, infectious, and recovered individuals over time with disease transmission
#' at each time point matching the pre-specified \eqn{R_t}. The function also
#' simulates the number of true and observed incident cases, convolving simulated
#' incidence through a specified delay distribution and simulating from a negative
#' binomial distribution.
#'
#' @param Rt A vector of time-varying reproduction number values. Default is a
#'  modified sine curve, oscillating between 0.50 and 1.50 with a period of 20 days.
#' @param S0 Initial number of susceptible individuals
#' @param I0 Initial number of infected individuals
#' @param R0 Initial number of recovered individuals
#' @param gamma Recovery rate, the inverse of the average infectious period
#' @param delay_pmf Probability mass function for delay distribution.
#'   Default is a vector with mean 3.
#' @param k Dispersion parameter for negative binomial distribution
#' @param seed Random seed for reproducibility
#' @param date0 Start date for the simulation. Only used to add dates to outputs for use
#'   with `RtGam()` and friends.
#' @param day_of_week Values to use for the day of week effect. Defaults to NULL for
#'    no effect.
#' @return A list containing the time series of S, I, R, beta, true cases,
#'   true Rt, observed cases, and reference dates
#' @export
simulate_sir <- function(
    Rt = 0.5 * sin(seq_len(100) / 10 * pi) + 1,
    S0 = 99000,
    I0 = 1000,
    R0 = 0,
    gamma = 0.5,
    delay_pmf = c(0.1, 0.2, 0.3, 0.4),
    k = 10,
    seed = 12345,
    date0 = as.Date("2023-01-01"),
    day_of_week = FALSE) {
  # We don't see new incidence on the last day of the simulation
  n_inc <- length(Rt)
  n_sim <- n_inc + 1

  S <- integer(n_sim)
  I <- integer(n_sim)
  R <- integer(n_sim)
  beta <- double(n_inc)
  incidence <- integer(n_inc)

  S[1] <- S0
  I[1] <- I0
  R[1] <- R0
  N <- S0 + I0 + R0

  for (t in 1:n_inc) {
    # Catch case where S is zero causing beta to be NA
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

    # Incidence is the number of new cases, I is the prevalence
    incidence[t] <- -dSdt
  }


  true_cases_full <- stats::convolve(incidence, rev(delay_pmf), type = "open")
  # TODO: Add day of week
  # Drop initial values biased by missing entries in the convolution
  true_cases <- true_cases_full[length(delay_pmf):length(true_cases_full)]
  withr::with_seed(seed, {
    obs_cases <- stats::rnbinom(
      n = length(true_cases),
      mu = true_cases,
      size = k
    )
  })

  # Observation events are shifted over by the length of the delay distribution
  # because of the dropped, biased values in the convolution
  sim_scale_dates <- date0 + 0:(n_sim - 1)
  incidence_scale_dates <- date0 + 0:(n_inc - 1)
  observation_scale_dates <- date0 + (length(delay_pmf)):(length(true_cases_full))

  result <- data.frame(
    reference_date = c(
      rep(sim_scale_dates, 3),
      rep(incidence_scale_dates, 2),
      rep(observation_scale_dates, 2)
    ),
    parameter = c(
      rep(c("S", "I", "R"), each = n_sim),
      rep(c("true_rt", "incident_infections"), each = n_inc),
      rep(
        c("true_incident_cases", "observed_incident_cases"),
        each = length(observation_scale_dates)
      )
    ),
    value = c(S, I, R, Rt, incidence, true_cases, obs_cases)
  )

  return(result)
}
