test_that("Dates are converted to strings", {
  dates_string <- c("2023-01-01", "2023-01-02")
  dates <- as.Date(dates_string)
  expect_equal(stringify_date(dates), dates_string)

  # Passes if not a `Date`
  expect_equal(stringify_date(dates_string), dates_string)
})


test_that("Works with a simple PMF vector", {
  pmf <- c(0.2, 0.3, 0.5)
  result <- truncate_and_renormalize(pmf)
  expect_equal(result, c(0, 0.375, 0.625))
  expect_equal(sum(result), 1)
})

test_that("Works when the first element is already zero", {
  pmf <- c(0, 0.4, 0.6)
  result <- truncate_and_renormalize(pmf)
  expect_equal(result, c(0, 0.4, 0.6))
  expect_equal(sum(result), 1)
})

test_that("Works with a different tolerance value", {
  pmf <- c(0.1, 0.2, 0.7)
  result <- truncate_and_renormalize(pmf, tol = 1e-5)
  expect_equal(result, c(0, 0.2222222, 0.7777778), tolerance = 1e-5)
  expect_equal(sum(result), 1, tolerance = 1e-5)
})
