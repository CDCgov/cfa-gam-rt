test_that("Vector equal length check works", {
  # Without group
  expect_error(
    check_vectors_equal_length(
      cases = 0:4,
      reference_date = 0:3,
      group = NULL
    ),
    class = "RtGam_invalid_input"
  )
  expect_null(
    check_vectors_equal_length(
      cases = 0:4,
      reference_date = 0:4,
      group = NULL
    )
  )
  # With group
  expect_error(
    check_vectors_equal_length(
      cases = 0:4,
      reference_date = 0:4,
      group = 0:3
    ),
    class = "RtGam_invalid_input"
  )
  expect_null(
    check_vectors_equal_length(
      cases = 0:4,
      reference_date = 0:4,
      group = 0:4
    )
  )
})

test_that("Unique date check works", {
  # Without group
  expect_error(
    check_dates_unique(
      as.Date(c("2023-01-01", "2023-01-01")),
      NULL
    ),
    class = "RtGam_invalid_input"
  )
  expect_null(
    check_dates_unique(
      as.Date(c("2023-01-01", "2023-01-02")),
      NULL
    )
  )

  # With group
  expect_error(
    check_dates_unique(
      as.Date(c("2023-01-01", "2023-01-01", "2023-01-02")),
      group = c(1, 1, 2)
    ),
    class = "RtGam_invalid_input",
  )
  expect_null(
    check_dates_unique(
      as.Date(c("2023-01-01", "2023-01-02", "2023-01-01")),
      group = c(1, 1, 2)
    )
  )
})

test_that("Required input check works", {
  cases <- c(1, 2)
  reference_date <- as.Date(c(1, 2))
  group <- c(1, 2)

  expect_error(
    check_required_inputs_provided(
      call = NULL
    ),
    class = "rlang_error"
  )
  expect_error(
    check_required_inputs_provided(
      cases = cases,
      call = NULL
    ),
    class = "rlang_error"
  )
  expect_error(
    check_required_inputs_provided(
      cases = cases,
      reference_date = reference_date,
      call = NULL
    ),
    class = "rlang_error"
  )
  expect_null(check_required_inputs_provided(cases,
    reference_date,
    group,
    call = NULL
  ))
})

test_that("Missingness check works", {
  no_missingness <- c(1, 2, 3)
  missingness <- c(NA, 1, 2)

  expect_error(check_no_missingness(missingness),
    class = "RtGam_invalid_input"
  )
  expect_null(check_no_missingness(no_missingness))
})

test_that("Negative element check works", {
  non_neg <- c(0, 1, 2)
  has_neg <- c(-1, 1, 2)
  call <- NULL
  arg <- "test"

  expect_error(check_elements_non_neg(has_neg, arg, call),
    class = "RtGam_invalid_input"
  )
  expect_null(check_elements_non_neg(non_neg, arg, call))
})

test_that("Sums to one check works", {
  sums_to_one <- c(0.2, 0.4, 0.4)
  sums_to_more <- c(0.2, 0.4, 0.405)
  call <- NULL
  arg <- "test"

  expect_error(check_sums_to_one(sums_to_more, arg, call),
    class = "RtGam_invalid_input"
  )
  expect_null(check_sums_to_one(sums_to_one, arg, call))
})

test_that("Type-checking dates works", {
  dates <- as.Date(c("2023-01-01", "2023-01-02"))
  not_dates <- c("abc", "123")
  arg <- "test"
  call <- NULL

  expect_error(check_date(not_dates, arg, call),
    class = "RtGam_type_error",
    regexp = "Date"
  )
  expect_null(check_date(dates, arg, call))
})

test_that("Type-checking vectors works", {
  bare_vector <- c(1, 2, 3)
  non_bare_vector <- as.Date(bare_vector)
  non_vector <- list()
  arg <- "test"
  call <- NULL

  expect_error(check_vector(non_bare_vector, arg, call),
    class = "RtGam_type_error",
    regexp = "vector"
  )
  expect_error(check_vector(non_vector, arg, call),
    class = "RtGam_type_error",
    regexp = "vector"
  )
  expect_null(check_vector(bare_vector, arg, call))
})

test_that("Type-checking integers works", {
  integers <- c(1L, 2L, 3L)
  integerish <- c(-1, 0, 1)
  not_integers <- c("a", "b", "c")
  tricky_not_integers <- as.Date(integers)
  arg <- "test"
  call <- NULL

  expect_error(check_integer(not_integers, arg, call),
    class = "RtGam_type_error",
    regexp = "integer"
  )

  expect_error(check_integer(tricky_not_integers, arg, call),
    class = "RtGam_type_error",
    regexp = "integer"
  )
  expect_null(check_integer(integers, arg, call))
  expect_null(check_integer(integerish, arg, call))
})

test_that("Type-checking strings works", {
  char <- c("a", "b", "c")
  non_char <- c(1, 2, 3)
  arg <- "test"
  call <- NULL

  expect_error(check_character(non_char, arg, call),
    class = "RtGam_type_error",
    regexp = "character"
  )
  expect_null(check_character(char, arg, call))
})

test_that("Type error throws successfully", {
  object <- "Test string"
  arg_name <- "Test argument"
  expected_type <- "integer"
  # Turns off call env -- we just want to check it throws
  call <- NULL

  expect_error(
    obj = throw_type_error(object, arg_name, expected_type, call),
    class = "RtGam_type_error"
  )
})
