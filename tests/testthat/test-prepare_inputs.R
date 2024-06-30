test_that("Dataframe constructed appropriately", {
  cases <- c(1L, 2L, 3L)
  reference_date <- as.Date(c("2023-01-01", "2023-01-02", "2023-01-03"))
  timestep <- c(0, 0.5, 1)

  # Without groups
  expected <- data.frame(
    cases = cases,
    timestep = c(0, 0.5, 1),
    reference_date = reference_date,
    group = rep(NA, 3)
  )
  actual <- prepare_inputs(cases, reference_date, NULL)
  expect_equal(actual, expected)

  # With groups
  group <- c(1, 1, 2)
  expected <- data.frame(
    cases = cases,
    timestep = c(0, 0.5, 1),
    reference_date = reference_date,
    group = group
  )
  actual <- prepare_inputs(cases, reference_date, group)
  expect_equal(actual, expected)
})

test_that("Date conversion matches expected", {
  dates <- seq.Date(
    from = as.Date("2023-01-01"),
    by = 1, # in days
    length.out = 5
  )
  min_date <- min(dates)
  max_date <- max(dates)
  expected <- c(0, 0.25, 0.5, 0.75, 1)

  actual <- dates_to_timesteps(dates, min_date, max_date)
  expect_equal(actual, expected)
})

test_that("Converts double vectors to integers with a warning", {
  double_vec <- c(1, 2, 3)
  integer_vec <- c(1L, 2L, 3L)

  expect_warning(actual <- integerify_cases(double_vec),
    regexp = "Coercing"
  )
  expect_equal(actual, integer_vec)
  expect_no_message(integerify_cases(integer_vec))
  expect_equal(integerify_cases(integer_vec), integer_vec)
})
