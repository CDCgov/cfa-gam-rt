test_that("RtGam parses inputs successfully", {
  timesteps <- 20
  cases <- 1:timesteps
  dates <- as.Date("2023-01-01") + 1:timesteps
  group <- NULL

  expect_null(RtGam(cases, timesteps, group))
})
