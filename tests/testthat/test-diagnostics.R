test_that("check_diagnostics() runs cleanly on happy path", {
  withr::with_seed(12345, {
    df <- data.frame(
      x = 1:20,
      y = rnorm(20, 1:20)
    )
    model <- mgcv::gam(
      formula = as.formula("y ~ 1 + s(x)"),
      data = df
    )
  })

  fit <- list(diagnostics = calculate_diagnostics(model))
  expected_diagnostics <- c(
    "model_converged",
    "k_prime",
    "k_edf",
    "k_index",
    "k_p_value",
    "k_to_edf_ratio",
    "residual_autocorrelation"
  )

  expect_equal(names(check_diagnostics(fit)), expected_diagnostics)
  expect_invisible(check_diagnostics(fit))
  expect_no_message(check_diagnostics(fit))
})

test_that("check_diagnostics() runs throws warnings for a bad fit", {
  fit <- RtGam::RtGam(
    cases = c(1L, 2L, 3L),
    reference_date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03")),
    warn_for_diagnostic_failure = FALSE
  )
  expected_diagnostics <- c(
    "model_converged",
    "k_prime",
    "k_edf",
    "k_index",
    "k_p_value",
    "k_to_edf_ratio",
    "residual_autocorrelation"
  )

  suppressMessages(expect_condition(
    check_diagnostics(fit, warn_for_diagnostic_failure = TRUE),
    regexp = "Residual autocorrelation present"
  ))
})

test_that("check_diagnostics() can be silenced", {
  fit <- RtGam::RtGam(
    cases = c(1L, 2L, 3L),
    reference_date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03")),
    warn_for_diagnostic_failure = FALSE
  )
  expected_diagnostics <- c(
    "model_converged",
    "k_prime",
    "k_edf",
    "k_index",
    "k_p_value",
    "k_to_edf_ratio",
    "residual_autocorrelation"
  )

  expect_no_condition(
    check_diagnostics(fit, warn_for_diagnostic_failure = FALSE)
  )
})

test_that("calculate_diagnostics returns expected diagnostics ", {
  withr::with_seed(12345, {
    df <- data.frame(
      x = 1:20,
      y = rnorm(20, 1:20)
    )
    fit <- mgcv::gam(
      formula = as.formula("y ~ 1 + s(x)"),
      data = df
    )
  })

  diagnostics <- calculate_diagnostics(fit)
  expected_diagnostics <- c(
    "model_converged",
    "k_prime",
    "k_edf",
    "k_index",
    "k_p_value",
    "k_to_edf_ratio",
    "residual_autocorrelation"
  )

  expect_true(inherits(diagnostics, "list"))
  expect_equal(names(diagnostics), expected_diagnostics)
  expect_true(diagnostics[["model_converged"]])
  expect_true(diagnostics[["k_index"]] > 1)
  expect_true(diagnostics[["k_p_value"]] > 0.05)
  expect_true(all(abs(diagnostics[["residual_autocorrelation"]]) < 0.5))
})

test_that("Warnings are not issued for clean diagnostics", {
  diagnostics <- list(
    model_converged = TRUE,
    k_edf = 2,
    k_index = 1.2,
    k_p_value = 0.81,
    k_to_edf_ratio = 0.1,
    residual_autocorrelation = c(-0.2, 0.0)
  )

  expect_no_condition(issue_diagnostic_warnings(diagnostics))
})

test_that("Warnings are issued for diagnostic failures", {
  no_convergence <- list(
    model_converged = FALSE,
    k_edf = 2,
    k_index = 1.2,
    k_p_value = 0.81,
    k_to_edf_ratio = 0.1,
    residual_autocorrelation = c(-0.2, 0.0)
  )
  k_to_edf_ratio_high <- list(
    model_converged = TRUE,
    k_prime = 9,
    k_edf = 2.0,
    k_index = 1.2,
    k_p_value = 0.81,
    k_to_edf_ratio = 1.0,
    residual_autocorrelation = c(-0.2, 0.0)
  )
  k_p_value_low <- list(
    model_converged = FALSE,
    k_edf = 2,
    k_index = 1.2,
    k_p_value = 0.01,
    k_to_edf_ratio = 0.1,
    residual_autocorrelation = c(-0.2, 0.0)
  )
  has_residual_autocorrelation <- list(
    model_converged = FALSE,
    k_edf = 2,
    k_index = 1.2,
    k_p_value = 0.81,
    k_to_edf_ratio = 0.1,
    residual_autocorrelation = c(0.8, 0.0)
  )

  expect_condition(
    issue_diagnostic_warnings(no_convergence),
    regexp = "Model failed to converge. Inference is not reliable."
  )
  # Wrapping in suppressMessages to catch all of multi-line warnings
  suppressMessages(expect_condition(
    issue_diagnostic_warnings(k_to_edf_ratio_high),
    regexp = "Effective degrees of freedom is near the supplied upper bound"
  ))
  suppressMessages(expect_condition(
    issue_diagnostic_warnings(k_p_value_low),
    regexp = "k-index for one or more smooths is below 1"
  ))
  suppressMessages(expect_condition(
    issue_diagnostic_warnings(has_residual_autocorrelation),
    regexp = "Residual autocorrelation present"
  ))
})
