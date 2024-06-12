test_that("Formula created more than 3 weeks", {
  n_timesteps <- 7 * 3
  k <- 10
  is_grouped <- FALSE
  expected <- "cases ~ 1 + s(timesteps, k = 10, m = 2, bs = \"ad\")"

  f <- formula_creator(n_timesteps, k, is_grouped)

  expect_type(f, "language")
  expect_equal(expected, deparse(f))
})

test_that("Formula created fewer than 3 weeks", {
  n_timesteps <- 7 * 3 - 1
  k <- 10
  is_grouped <- FALSE
  expected <- "cases ~ 1 + s(timesteps, k = 10, bs = \"tp\")"


  f <- formula_creator(n_timesteps, k, is_grouped)

  expect_type(f, "language")
  expect_equal(expected, deparse(f))
})

test_that("Penalty basis dim created successfully", {
  expect_equal(1L, penalty_basis_creator(3))
  expect_equal(1L, penalty_basis_creator(20))
  expect_equal(2L, penalty_basis_creator(21))
  expect_equal(3L, penalty_basis_creator(60))
})

test_that("Smooth basis dim created successfully", {
  k <- 15

  smooth_basis_dim <- smooth_basis_creator(k)

  expect_type(smooth_basis_dim, "list")
  expect_equal(c("global_trend"), names(smooth_basis_dim))
  expect_equal(15, smooth_basis_dim[["global_trend"]])
})
