pkgload::load_all()
data <- stochastic_sir_rt[41:100, ]
fit <- RtGam(
  cases = data[["obs_cases"]],
  reference_date = data[["reference_date"]],
  day_of_week = TRUE
)
saveRDS(fit, "tests/testthat/data/stochastic_sir_fit.rds")
