test_that("simulate_sir returns a list with correct elements", {
  result <- simulate_sir()
  expect_type(result, "list")
  expect_setequal(
    unique(result[["parameter"]]),
    c(
      "S",
      "I",
      "R",
      "true_rt",
      "incident_infections",
      "true_incident_cases",
      "observed_incident_cases"
    )
  )
})

test_that("simulate_sir handles zero initial infections", {
  result <- simulate_sir(I0 = 0)
  expect_true(all(result[which(result$parameter == "I"), "value"] == 0))
})

test_that("simulate_sir handles zero initial susceptibles", {
  result <- simulate_sir(S0 = 0)
  expect_true(all(result[which(result$parameter == "S"), "value"] == 0))
})

test_that("simulate_sir handles exhausting susceptibles", {
  result <- simulate_sir(Rt = rep(2, 20), S0 = 999, I0 = 1, R0 = 0)
  expect_true(all(result[which(result$parameter == "Rt"), "value"] == 2))
  expect_true(all(result[which(result$parameter == "S"), "value"] >= 0))
})
