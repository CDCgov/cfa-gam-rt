#' Simulate forward process model
simulate_sir <- function(
    S0 = 9900,
    I0 = 100,
    R0 = 0,
    Rt = 0.5 * sin(seq_len(100) / 30 * pi) + 1,
    gamma = 0.1,
    delay_pmf = c(rep(0, 19), 1),
    k = 100,
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

  for (t in 1:(length(beta) - 1)) {
    beta[t] <- (Rt[t] / S[t]) * gamma
    dSdt <- -max(as.integer(beta[t] * S[t] * I[t]), 0L)
    dRdt <- max(as.integer(I[t] * gamma), 0)
    dIdt <- -dSdt - dRdt

    S[t + 1] <- S[t] + dSdt
    I[t + 1] <- I[t] + dIdt
    R[t + 1] <- R[t] + dRdt
  }

  true_cases <- convolve(I, rev(delay_pmf), type = "open")
  obs_cases <- rnbinom(
    n = length(Rt),
    mu = true_cases,
    size = k
  )

  return(list(
    S = S,
    I = I,
    R = R,
    true_cases = true_cases,
    true_rt = Rt,
    obs_cases = obs_cases,
    reference_date = date_0 + 0:(length(Rt) - 1)
  ))
}
