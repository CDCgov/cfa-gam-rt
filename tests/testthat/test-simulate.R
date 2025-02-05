test_that("simulate_sir returns a list with correct elements", {
  result <- simulate_sir()
  expect_type(result, "list")
  expect_named(
    result,
    c("S", "I", "R", "beta", "true_cases", "true_rt", "obs_cases", "reference_date")
  )
})

test_that("simulate_sir handles zero initial infections", {
  result <- simulate_sir(I0 = 0)
  expect_equal(result$I[1], 0)
  expect_equal(result$S[1], 99000)
  expect_equal(result$R[1], 0)
})

test_that("simulate_sir handles zero initial susceptibles", {
  result <- simulate_sir(S0 = 0)
  expect_equal(result$S[1], 0)
  expect_equal(result$I[1], 1000)
  expect_equal(result$R[1], 0)
})
