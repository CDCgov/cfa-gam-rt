test_that("Dates are shifted correctly", {
  # 3 branches in the code:
  # 1. obs_cases: should return the same as the desired min and max
  # 2. inc/growth: should return mean-delay-shifted timesteps
  # 3. Rt: should return mean-delay-shifted GI-length-padded timesteps

  # Arrange
  desired_min_date <- as.Date("2023-01-10")
  desired_max_date <- as.Date("2023-01-15")
  gi_pmf <- c(0.1, 0.2)
  mean_delay <- 5

  # Test case (1): `obs_cases` has no shift
  expected_cases_dates <- seq.Date(
    from = desired_min_date,
    to = desired_max_date,
    by = "day"
  )
  actual_cases_dates <- shift_desired_dates(
    type = "obs_cases",
    desired_min_date = desired_min_date,
    desired_max_date = desired_max_date,
    mean_delay = mean_delay,
    gi_pmf = gi_pmf
  )
  expect_equal(actual_cases_dates, expected_cases_dates)

  # Test case (2): incidence and growth rate has a mean shift
  expected_incidence_dates <- seq.Date(
    from = desired_min_date + mean_delay,
    to = desired_max_date + mean_delay,
    by = "day"
  )
  actual_incidence_dates <- shift_desired_dates(
    type = "incidence",
    desired_min_date = desired_min_date,
    desired_max_date = desired_max_date,
    mean_delay = mean_delay,
    gi_pmf = gi_pmf
  )
  actual_growth_dates <- shift_desired_dates(
    type = "r",
    desired_min_date = desired_min_date,
    desired_max_date = desired_max_date,
    mean_delay = mean_delay,
    gi_pmf = gi_pmf
  )
  expect_equal(actual_incidence_dates, expected_incidence_dates)
  expect_equal(actual_growth_dates, expected_incidence_dates)

  # Test case (3): Rt has a delay shift and GI length padding
  gi_length <- length(gi_pmf)
  expected_rt_dates <- seq.Date(
    from = desired_min_date + mean_delay - gi_length,
    to = desired_max_date + mean_delay + gi_length,
    by = "day"
  )
  actual_rt_dates <- shift_desired_dates(
    type = "Rt",
    desired_min_date = desired_min_date,
    desired_max_date = desired_max_date,
    mean_delay = mean_delay,
    gi_pmf = gi_pmf
  )
  expect_equal(actual_rt_dates, expected_rt_dates)
})


test_that("Dates are parsed correctly", {
  # Passing just `object` gives the same min and max dates
  expected_object_dates <- seq.Date(
    from = as.Date("2023-01-01"),
    to = as.Date("2023-01-15"),
    by = "day"
  )
  object_dates <- list(
    min_date = as.Date("2023-01-01"),
    max_date = as.Date("2023-01-15")
  )
  actual_object_dates <- parse_predict_dates(
    object = object_dates
  )
  expect_equal(
    actual_object_dates,
    expected_object_dates
  )

  # Passing horizon gives you n day ahead forcast
  horizon <- 5
  expected_horizon_dates <- seq.Date(
    from = object_dates$max_date + 1,
    to = object_dates$max_date + 1 + horizon,
    by = "day"
  )

  actual_horizon_dates <- parse_predict_dates(
    object = object_dates,
    horizon = horizon
  )
  expect_equal(actual_horizon_dates, expected_horizon_dates)

  # Horizon + min_date gives period from min_date to horizon
  object_dates <- list(
    min_date = as.Date("2023-01-01"),
    max_date = as.Date("2023-01-15")
  )
  min_date <- as.Date("2023-01-10")
  horizon <- 3
  expected_min_horiz_dates <- seq.Date(
    from = min_date,
    to = object_dates$max_date + 1 + horizon,
    by = "day"
  )
  actual_min_horiz_dates <- parse_predict_dates(
    object = object_dates,
    horizon = horizon,
    min_date = min_date
  )
  expect_equal(actual_min_horiz_dates, expected_min_horiz_dates)

  # Specifying min and max dates returns from min to max
  object_dates <- list(
    min_date = as.Date("2023-01-01"),
    max_date = as.Date("2023-01-15")
  )
  min_date <- as.Date("2023-01-02")
  max_date <- as.Date("2023-01-20")
  expected_min_max_dates <- seq.Date(
    from = min_date,
    to = max_date,
    by = "day"
  )
  actual_min_max_dates <- parse_predict_dates(
    object = object,
    min_date = min_date,
    max_date = max_date
  )
  expect_equal(actual_min_max_dates, expected_min_max_dates)
})

test_that("Bad dates throw appropriate status messages", {
  object <- list(
    min_date = as.Date("2023-01-01"),
    max_date = as.Date("2023-01-15")
  )
  # Type errors
  expect_error(
    parse_predict_dates(object, max_date = "potato"),
    class = "RtGam_type_error"
  )
  expect_error(
    parse_predict_dates(object, min_date = "potato"),
    class = "RtGam_type_error"
  )
  expect_error(
    parse_predict_dates(object, horizon = "potato"),
    class = "RtGam_type_error"
  )

  # Specifying both horizon and max date
  expect_error(
    parse_predict_dates(list(), horizon = 15, max_date = as.Date("2023-01-01"))
  )

  # min_date after max_date
  min_date <- as.Date("2023-01-01")
  max_date <- min_date + 1
  expect_snapshot(
    actual <- parse_predict_dates(
      list(),
      # Swap min and max to invoke warning
      min_date = max_date,
      max_date = min_date
    )
  )
  expect_equal(
    actual,
    seq.Date(from = min_date, to = max_date, by = "day")
  )
})
