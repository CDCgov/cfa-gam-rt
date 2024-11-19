test_that("Plots look the same", {
  fit <- readRDS(test_path("data", "stochastic_sir_fit.rds"))

  # Obs cases
  p <- plot(fit)
  vdiffr::expect_doppelganger("obs_cases", p)

  # r
  p <- plot(fit, parameter = "r", mean_delay = 0)
  vdiffr::expect_doppelganger("r", p)

  # Rt
  p <- plot(fit, parameter = "Rt", mean_delay = 0, gi_pmf = sir_gt_pmf)
  vdiffr::expect_doppelganger("Rt", p)

  # Can just plot forecast
  p <- plot(fit, horizon = 10)
  vdiffr::expect_doppelganger("forecast", p)
})
