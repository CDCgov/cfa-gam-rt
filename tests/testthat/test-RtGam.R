test_that("RtGam parses inputs successfully", {
  timesteps <- 20
  cases <- 1:timesteps
  dates <- as.Date("2023-01-01") + 1:timesteps
  group <- NULL

  expect_null(RtGam(cases, dates, group))
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
