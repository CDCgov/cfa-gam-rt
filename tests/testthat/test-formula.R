test_that("Formula created more than 3 weeks", {
  k <- 10
  m <- 2
  is_grouped <- FALSE
  expected <- "cases ~ 1 + s(timesteps, k = 10, m = 2, bs = \"ad\")"

  f <- formula_creator(k, m, is_grouped)

  expect_type(f, "language")
  expect_equal(expected, deparse(f))
})

test_that("Formula created fewer than 3 weeks", {
  k <- 10
  m <- 1
  is_grouped <- FALSE
  expected <- "cases ~ 1 + s(timesteps, k = 10, bs = \"tp\")"


  f <- formula_creator(k, m, is_grouped)

  expect_type(f, "language")
  expect_equal(expected, deparse(f))
})

test_that("Smooth basis dim created successfully", {
  k <- 15

  smooth_basis_dim <- smooth_basis_creator(k)

  expect_type(smooth_basis_dim, "list")
  expect_equal(c("global_trend"), names(smooth_basis_dim))
  expect_equal(15, smooth_basis_dim[["global_trend"]])
})
