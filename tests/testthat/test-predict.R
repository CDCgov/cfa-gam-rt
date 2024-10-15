test_that("Dates are parsed correctly", {
  # Passing just `object` gives the same min and max dates
  expected_object_dates <- list(
    min_date = as.Date("2023-01-01"),
    max_date = as.Date("2023-01-15")
  )
  actual_object_dates <- parse_predict_dates(
    object = expected_object_dates
  )
  expect_equal(
    actual_object_dates,
    expected_object_dates
  )

  # Passing horizon gives you n day ahead forcast
  object_dates <- list(
    min_date = as.Date("2023-01-01"),
    max_date = as.Date("2023-01-15")
  )
  horizon <- 5
  expected_horizon_dates <- list(
    min_date = object_dates$max_date + 1,
    max_date = object_dates$max_date + 1 + horizon
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
  expected_min_horiz_dates <- list(
    min_date = min_date,
    max_date = object_dates$max_date + 1 + horizon
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
  expected_min_max_dates <- list(
    min_date = min_date,
    max_date = max_date
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
  expect_warning(
    actual <- parse_predict_dates(
      list(),
      # Swap min and max to invoke warning
      min_date = max_date,
      max_date = min_date
    ),
    class = "RtGam_predict_dates_backward"
  )
  expect_equal(
    actual,
    list(min_date = min_date, max_date = max_date)
  )
})
