test_that("`validate()` is successful", {
  cases <- c(1, 2, 3)
  dates <- as.Date(c("2023-01-01", "2023-01-02", "2023-01-03"))
  groups <- NULL
  k <- 3
  m <- 1

  expect_null(validate(cases, dates, groups, k, m))
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

  expect_null(validate_dates(dates, "test"))
  expect_error(validate_dates(not_dates, "test"),
    class = "RtGam_type_error"
  )
  expect_error(validate_dates(has_missing, "test"),
    class = "RtGam_invalid_input"
  )
  expect_error(validate_dates(not_a_vector, "test"),
    class = "RtGam_type_error"
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
