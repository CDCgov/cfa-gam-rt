test_that("Plots look the same", {
  fit <- readRDS(test_path("data", "fit.rds"))

  # Obs cases
  p <- plot(fit)
  expect_snapshot_file(save_plot(p, "obs_cases.png"))

  # r
  p <- plot(fit, parameter = "r", mean_delay = 0)
  expect_snapshot_file(save_plot(p, "r.png"))

  # Rt
  p <- plot(fit, parameter = "Rt", mean_delay = 0, gi_pmf = sir_gt_pmf)
  expect_snapshot_file(save_plot(p, "Rt.png"))

  # Can just plot forecast
  p <- plot(fit, horizon = 10)
  expect_snapshot_file(save_plot(p, "forecast.png"))
})
