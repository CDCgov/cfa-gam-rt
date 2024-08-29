test_that("RtGam parses inputs successfully", {
  timesteps <- 20
  withr::with_seed(12345, {
    cases <- rpois(20, 10)
  })
  dates <- as.Date("2023-01-01") + 1:timesteps
  group <- NULL

  fit <- RtGam(cases, dates, group)
  expected_slots <- c(
    "model",
    "data",
    "min_date",
    "max_data",
    "k",
    "m",
    "backend",
    "formula",
    "diagnostics"
  )

  expect_s3_class(fit, "RtGam")
  expect_equal(names(fit), expected_slots)
})

test_that("Non-supported backends throw error", {
  timesteps <- 20
  cases <- 1:timesteps
  dates <- as.Date("2023-01-01") + 1:timesteps
  group <- NULL

  expect_error(RtGam(cases, dates, group, backend = "test"),
    class = "RtGam_invalid_input"
  )
})
