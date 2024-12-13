test_that("`validate()` is successful", {
  cases <- c(1, 2, 3)
  dates <- as.Date(c("2023-01-01", "2023-01-02", "2023-01-03"))
  day_of_week <- TRUE
  groups <- NULL
  k <- 3
  m <- 1

  expect_equal(
    validate(
      cases = cases,
      reference_date = dates,
      group = groups,
      day_of_week = day_of_week,
      k = k,
      m = m
    ),
    dates
  )
})

test_that("Wrong length fails", {
  cases <- c(1, 2, 3)
  dates <- as.Date(c("2023-01-01", "2023-01-02", "2023-01-03"))
  day_of_week <- c(1, 2)
  groups <- NULL
  k <- 3
  m <- 1

  expect_snapshot(
    validate(
      cases = cases,
      reference_date = dates,
      group = groups,
      day_of_week = day_of_week,
      k = k,
      m = m
    ),
    error = TRUE
  )
})


test_that("`validate_cases()` is successful", {
  cases <- c(1, 2, 3)
  cases_with_missing <- c(0, 2, 3, NA)
  wrong_type <- as.Date(c(0, 2, 3))
  not_a_vector <- list(cases)
  has_neg <- c(-1, cases)

  expect_null(validate_cases(cases))
  expect_error(validate_cases(cases_with_missing),
    class = "RtGam_invalid_input"
  )
  expect_error(validate_cases(not_a_vector),
    class = "RtGam_type_error"
  )
  expect_error(validate_cases(wrong_type),
    class = "RtGam_type_error"
  )
  expect_error(validate_cases(has_neg),
    class = "RtGam_invalid_input"
  )
})

test_that("`validate_dates()` is successful", {
  dates <- as.Date(c(1, 2, 3))
  not_dates <- c(1, 2, 3)
  has_missing <- c(dates, NA)
  not_a_vector <- list(dates)

  expect_equal(validate_dates(dates, "test"), dates)
  expect_error(validate_dates(not_dates, "test"),
    class = "RtGam_type_error"
  )
  expect_error(validate_dates(has_missing, "test"),
    class = "RtGam_invalid_input"
  )
  expect_snapshot(validate_dates(not_a_vector, "test"),
    error = TRUE
  )
})

test_that("`validate_group()` is successful", {
  expect_error(validate_group(c(1, 2, 3), call = NULL),
    class = "RtGam_not_implemented"
  )
  expect_null(validate_group(NULL, call = NULL))
})

test_that("`validate_min_dimensionality()` is successful", {
  not_a_vector <- list("a" = 1)
  not_an_integer <- as.Date(1)
  missing <- c(NA)
  below_min <- 1L
  too_long <- c(1, 2)
  too_short <- c()
  good_input <- 3
  min_dim <- 3
  max_val <- 0

  expect_error(validate_min_dimensionality(not_a_vector, "test", min_dim),
    class = "RtGam_type_error"
  )
  expect_error(validate_min_dimensionality(not_an_integer, "test", min_dim),
    class = "RtGam_type_error"
  )
  expect_error(validate_min_dimensionality(missing, "test", min_dim),
    class = "RtGam_invalid_input"
  )
  expect_error(validate_min_dimensionality(below_min, "test", min_dim),
    class = "RtGam_invalid_input"
  )
  expect_error(validate_min_dimensionality(below_min, "test", min_dim),
    class = "RtGam_invalid_input"
  )
  expect_error(validate_min_dimensionality(too_long, "test", min_dim),
    class = "RtGam_invalid_input"
  )
  expect_error(validate_min_dimensionality(too_short, "test", min_dim),
    class = "RtGam_type_error"
  )
  expect_error(
    validate_min_dimensionality(1:5,
      "test",
      min_dim,
      max_val = 1
    ),
    class = "RtGam_invalid_input"
  )

  expect_null(validate_min_dimensionality(
    good_input,
    "test",
    min_dim
  ))
  expect_null(validate_min_dimensionality(good_input,
    "test",
    min_dim,
    max_val = 3
  ))
})

test_that("validate_day_of_week is successful", {
  # Case 1: Day of week is true
  expect_invisible(validate_day_of_week(TRUE))

  # Case 2: Day of week is false
  expect_invisible(validate_day_of_week(FALSE))

  # Case 3: Use a good vector
  expect_invisible(validate_day_of_week(c(1, 2)))

  # Case 4: Errors for a bad vector
  expect_snapshot(
    error = TRUE,
    # Can't have NAs
    validate_day_of_week(c(NA, "a"))
  )
})

test_that("validate_predict_inputs handles 'obs_cases' correctly", {
  expect_snapshot(
    validate_predict_inputs(
      parameter = "obs_cases",
      mean_delay = 2,
      gi_pmf = c(0.5, 0.5)
    )
  )

  expect_snapshot(
    validate_predict_inputs(
      parameter = "obs_cases",
      mean_delay = NULL,
      gi_pmf = c(0.5, 0.5)
    )
  )

  expect_silent(
    validate_predict_inputs(
      parameter = "obs_cases",
      mean_delay = NULL,
      gi_pmf = NULL
    )
  )
})

test_that(
  "validate_predict_inputs requires mean_delay for non-obs_cases parameters",
  {
    expect_snapshot(
      validate_predict_inputs(
        parameter = "r",
        mean_delay = NULL,
        gi_pmf = c(0.5, 0.5)
      ),
      error = TRUE
    )

    expect_silent(
      validate_predict_inputs(
        parameter = "r",
        mean_delay = 2,
        gi_pmf = c(0.5, 0.5)
      )
    )

    expect_silent(
      validate_predict_inputs(
        parameter = "Rt",
        mean_delay = 2,
        gi_pmf = c(0.5, 0.5)
      )
    )
  }
)

test_that("validate_predict_inputs requires gi_pmf for parameter 'Rt'", {
  expect_snapshot(
    expect_error(
      validate_predict_inputs(
        parameter = "Rt",
        mean_delay = 2,
        gi_pmf = NULL
      )
    )
  )
})

test_that("validate_predict_inputs checks gi_pmf values for 'Rt'", {
  expect_snapshot(
    validate_predict_inputs(
      parameter = "Rt",
      mean_delay = 2,
      gi_pmf = c(0.5, NA)
    ),
    error = TRUE
  )

  expect_snapshot(
    validate_predict_inputs(
      parameter = "Rt",
      mean_delay = 2,
      gi_pmf = c(-0.1, 1.1)
    ),
    error = TRUE
  )

  expect_silent(
    validate_predict_inputs(
      parameter = "Rt",
      mean_delay = 2,
      gi_pmf = c(0.5, 0.5)
    )
  )
})

test_that("validate_predict_inputs handles invalid parameter values", {
  expect_snapshot(
    validate_predict_inputs(
      parameter = "unknown",
      mean_delay = 2,
      gi_pmf = c(0.5, 0.5)
    ),
    error = TRUE
  )
})
