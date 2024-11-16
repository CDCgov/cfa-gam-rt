test_that("Plots look the same", {
  fit <- readRDS(test_path("data", "stochastic_sir_fit.rds"))

  # Obs cases
  p <- plot(fit)
  expect_snapshot_file(save_plot(p, "obs_cases.jpg"))

  # r
  p <- plot(fit, parameter = "r", mean_delay = 0)
  expect_snapshot_file(save_plot(p, "r.jpg"))

  # Rt
  p <- plot(fit, parameter = "Rt", mean_delay = 0, gi_pmf = sir_gt_pmf)
  expect_snapshot_file(save_plot(p, "Rt.jpg"))

  # Can just plot forecast
  p <- plot(fit, horizon = 10)
  expect_snapshot_file(save_plot(p, "forecast.jpg"))
})
