test_that("Dates are converted to strings", {
  dates_string <- c("2023-01-01", "2023-01-02")
  dates <- as.Date(dates_string)
  expect_equal(stringify_date(dates), dates_string)

  # Passes if not a `Date`
  expect_equal(stringify_date(dates_string), dates_string)
})
