#' Simulate forward process model
simulate_sir <- function(
    S0 = 9990,
    I0 = 10,
    R0 = 0,
    Rt = 0.5 * sin(seq_len(100) / 15 * pi) + 1,
    gamma = 0.1,
    delay_pmf = c(0.1, 0.1, 0.2, 0.4, 0.3),
    k = 15,
    seed = 12345) {
  S <- integer(length(Rt))
  I <- integer(length(Rt))
  R <- integer(length(Rt))
  beta <- double(length(Rt))

  S[1] <- S0
  I[1] <- I0
  R[1] <- R0

  incidence <- integer(length(Rt))
  gi_mean <- 1 # gamma rv, shape = 1, rate = 1

  for (t in 1:(length(beta) - 1)) {
    beta[t] <- Rt[t] / S[t]
    dSdt <- -max(as.integer(beta[t] * S[t] * I[t]), 0L)
    dRdt <- max(as.integer(I[t] * gamma), 0)
    dIdt <- -dSdt - dRdt

    S[t + 1] <- S[t] + dSdt
    I[t + 1] <- I[t] + dIdt
    R[t + 1] <- R[t] + dRdt
  }
  return(list(S = S, I = I, R = R))
}
