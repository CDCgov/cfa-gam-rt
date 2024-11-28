test_that("Dataframe constructed appropriately", {
  cases <- c(1L, 2L, 3L)
  reference_date <- as.Date(c("2023-01-01", "2023-01-02", "2023-01-03"))
  day_of_week <- FALSE
  timestep <- c(0, 0.5, 1)

  # Without groups
  expected <- data.frame(
    cases = cases,
    timestep = c(0, 0.5, 1),
    reference_date = reference_date,
    group = rep(NA, 3),
    day_of_week = FALSE
  )
  class(expected) <- c("RtGam_gam", class(expected))

  actual <- dataset_creator(
    cases = cases,
    reference_date = reference_date,
    group = NULL,
    day_of_week = day_of_week,
    backend = "gam"
  )
  expect_equal(actual, expected)

  # With groups
  group <- c(1, 1, 2)
  expected <- data.frame(
    cases = cases,
    timestep = c(0, 0.5, 1),
    reference_date = reference_date,
    group = group,
    day_of_week = FALSE
  )
  class(expected) <- c("RtGam_gam", class(expected))

  actual <- dataset_creator(cases,
    reference_date,
    group,
    day_of_week,
    backend = "gam"
  )
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

test_that("Day of week parsed appropriately", {
  reference_date <- as.Date(c("2023-01-01", "2023-01-02", "2023-01-3"))

  # Case 1: day_of_week is TRUE so dates are auto-parsed
  expected <- as.factor(format(reference_date, "%A"))
  expect_equal(
    set_day_of_week_factor(TRUE, reference_date),
    expected
  )

  # Case 2: day_of_week is FALSE -- rep vector of FALSE
  expected <- rep(FALSE, length(reference_date))
  expect_equal(
    set_day_of_week_factor(FALSE, reference_date),
    expected
  )

  # Case 3: Manual vector
  day_of_week_manual <- c("Weekday", "Weekend", "Holiday")
  expected <- as.factor(day_of_week_manual)

  expect_equal(
    set_day_of_week_factor(day_of_week_manual, reference_date),
    expected
  )
})
