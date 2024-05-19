test_that("`validate()` is successful", {
  cases <- c(1, 2, 3)
  dates <- as.Date(c("2023-01-01", "2023-01-02", "2023-01-03"))
  groups <- NULL

  expect_null(validate(cases, dates, groups))
})

test_that("`validate_cases()` is successful", {
  cases <- c(1, 2, 3)
  cases_with_missing <- c(0, 2, 3, NA)
  wrong_type <- as.Date(c(0, 2, 3))
  not_a_vector <- list(cases)
  has_neg <- c(-1, cases)

  expect_null(validate_cases(cases))
  expect_error(validate_cases(cases_with_missing, "test"),
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
