test_that("predict can handle string dates", {
  fit <- readRDS(test_path("data", "stochastic_sir_fit.rds"))

  # Throws properly formatted warnings
  expect_snapshot(
    actual <- predict(fit, min_date = "2023-01-05", max_date = "2023-01-10")
  )

  # Dates are correct
  expect_equal(min(actual[["reference_date"]]), as.Date("2023-01-05"))
  expect_equal(max(actual[["reference_date"]]), as.Date("2023-01-10"))
})

test_that("predict_obs_cases predicts observed cases", {
  fit <- readRDS(test_path("data", "stochastic_sir_fit.rds"))

  actual <- predict(fit,
    min_date = "2023-01-01",
    max_date = "2023-01-10",
    n = 2,
    day_of_week = TRUE
  )

  expect_equal(nrow(actual), 20)
  expect_setequal(colnames(actual), c("reference_date", ".response", ".draw"))
  expect_true(rlang::is_integer(actual[[".response"]]))
  expect_setequal(unique(actual[[".draw"]]), c(1, 2))
})

test_that("predicting little r is on correct scale", {
  fit <- readRDS(test_path("data", "fit.rds"))

  actual <- predict.RtGam(fit,
    n = 1,
    seed = 12345,
    parameter = "r",
    mean_delay = 3,
    horizon = 10
  )
  # Hard to know that this is right-right, so testing that it has reasonable
  # properties
  expect_equal(nrow(actual), 10)
  expect_equal(colnames(actual), c("reference_date", ".response", ".draw"))
  expect_true(all(abs(actual[[".response"]]) < 0.5))
  expect_true(
    length(
      unique(
        actual[["reference_date"]]
      )
    ) == length(actual[["reference_date"]])
  )
})

test_that("predicting Rt works and is reasonable", {
  fit <- readRDS(test_path("data", "fit.rds"))
  actual <- predict(fit,
    mean_delay = 2,
    gi_pmf = c(0.5, 0.5),
    horizon = 10,
    n = 1,
    parameter = "Rt"
  )

  expect_equal(nrow(actual), 10)
  expect_equal(colnames(actual), c("reference_date", ".response", ".draw"))
  expect_true(all(abs(actual[[".response"]]) > 0))
  expect_true(
    length(
      unique(
        actual[["reference_date"]]
      )
    ) == length(actual[["reference_date"]])
  )
})

test_that("Newdata dataframe generated correctly", {
  object <- readRDS(test_path("data", "fit.rds"))
  mean_delay <- 2
  gi_pmf <- c(0.5, 0.5)
  min_date <- object[["min_date"]]
  max_date <- object[["max_date"]]

  expected_cols <- c("timestep", ".row", "reference_date")
  expected_nrows <- 10
  # Obs cases
  actual <- create_newdata_dataframe(
    object = object,
    parameter = "obs_cases",
    min_date = NULL,
    max_date = NULL,
    horizon = NULL,
    day_of_week = FALSE,
    mean_delay = mean_delay
  )
  expect_equal(colnames(actual), expected_cols)
  expect_equal(nrow(actual), expected_nrows)

  # r
  actual <- create_newdata_dataframe(
    object = object,
    min_date = min_date,
    max_date = max_date,
    horizon = NULL,
    parameter = "r",
    mean_delay = mean_delay,
    delta = 0.1
  )
  expect_equal(colnames(actual), expected_cols)
  expect_equal(nrow(actual), 2 * expected_nrows)

  # Rt
  actual <- create_newdata_dataframe(
    object = object,
    min_date = min_date,
    max_date = max_date,
    horizon = NULL,
    parameter = "Rt",
    mean_delay = mean_delay,
    gi_pmf = gi_pmf
  )
  expect_equal(colnames(actual), expected_cols)
  expect_equal(nrow(actual), expected_nrows + 2 * length(gi_pmf))
})

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
    parameter = "obs_cases",
    desired_min_date = desired_min_date,
    desired_max_date = desired_max_date,
    mean_delay = mean_delay,
    gi_pmf = gi_pmf
  )
  expect_equal(actual_cases_dates, expected_cases_dates)

  # Test case (2): growth rate has a mean shift
  expected_incidence_dates <- seq.Date(
    from = desired_min_date + mean_delay,
    to = desired_max_date + mean_delay,
    by = "day"
  )
  actual_growth_dates <- shift_desired_dates(
    parameter = "r",
    desired_min_date = desired_min_date,
    desired_max_date = desired_max_date,
    mean_delay = mean_delay,
    gi_pmf = gi_pmf
  )
  expect_equal(actual_growth_dates, expected_incidence_dates)

  # Test case (3): Rt has a delay shift and GI length padding
  gi_length <- length(gi_pmf)
  expected_rt_dates <- seq.Date(
    from = desired_min_date + mean_delay - gi_length,
    to = desired_max_date + mean_delay + gi_length,
    by = "day"
  )
  actual_rt_dates <- shift_desired_dates(
    parameter = "Rt",
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
    to = object_dates$max_date + horizon,
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
    to = object_dates$max_date + horizon,
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

  expect_snapshot(
    actual_min_max_dates <- parse_predict_dates(
      object = object,
      min_date = min_date,
      max_date = max_date
    )
  )
  expect_equal(actual_min_max_dates, expected_min_max_dates)
})

test_that("String dates are parsed to dates", {
  object_dates <- list(
    min_date = as.Date("2023-01-01"),
    max_date = as.Date("2023-01-15")
  )
  min_date <- "2023-01-02"
  max_date <- "2023-01-20"
  expected_min_max_dates <- seq.Date(
    from = as.Date(min_date),
    to = as.Date(max_date),
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
    class = "failed_to_cast_date"
  )
  expect_error(
    parse_predict_dates(object, min_date = "potato"),
    class = "failed_to_cast_date"
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
