test_that("Adds day of week", {
  k <- 10
  m <- 2
  is_grouped <- FALSE
  day_of_week <- as.factor(c("a", "b"))
  expected <- as.formula("cases ~ 1 + s(timestep, k = 10, m = 2, bs = \"ad\") + s(day_of_week, bs = \"re\")") # nolint

  f <- formula_creator(k, m, is_grouped, day_of_week)

  expect_type(f, "language")
  expect_equal(deparse(f), deparse(expected))
})

test_that("Formula created more than 3 weeks", {
  k <- 10
  m <- 2
  is_grouped <- FALSE
  day_of_week <- c(NA, NA)
  expected <- as.formula("cases ~ 1 + s(timestep, k = 10, m = 2, bs = \"ad\")")

  f <- formula_creator(k, m, is_grouped, day_of_week)

  expect_type(f, "language")
  expect_equal(deparse(expected), deparse(f))
})

test_that("Formula created fewer than 3 weeks", {
  k <- 10
  m <- 1
  day_of_week <- c(NA, NA)
  is_grouped <- FALSE
  expected <- as.formula("cases ~ 1 + s(timestep, k = 10, bs = \"tp\")")


  f <- formula_creator(k, m, is_grouped, day_of_week)

  expect_type(f, "language")
  # formula and deparse to standardize whitespace
  expect_equal(deparse(expected), deparse(f))
})

test_that("Smooth basis dim created successfully", {
  k <- 15

  smooth_basis_dim <- smooth_basis_creator(k)

  expect_type(smooth_basis_dim, "list")
  expect_equal(c("global_trend"), names(smooth_basis_dim))
  expect_equal(15, smooth_basis_dim[["global_trend"]])
})


test_that("`warn_for_suboptimal_params()` throws warning", {
  dat <- data.frame(
    cases = 1:10,
    reference_date = seq.Date(
      from = as.Date("2023-12-1"),
      length.out = 10,
      by = "day"
    ),
    timepoint = 1:10 / 10
  )
  k <- 1

  expect_warning(warn_for_suboptimal_params(dat, k, m = 10))
  expect_null(warn_for_suboptimal_params(dat, k, m = 1))
})
