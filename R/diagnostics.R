#' Check quantitative diagnostics from a fitted RtGam model
#'
#' Evaluates for convergence, effective degrees of freedom, and residual
#' autocorrelation. If `warn_for_diagnostic_failure` is set to TRUE, will issue
#' warnings when potential diagnostic issues are detected. The diagnostics are
#' invisibly returned as a list and also stored within the `diagnostics` element
#' of the provided model object.
#'
#' @param fit A fitted `RtGam` model object. This should be the result of
#'   calling `RtGam::RtGam()` with appropriate data.
#' @param warn_for_diagnostic_failure A logical value indicating whether to
#'   issue warnings if diagnostic checks suggest potential issues with the model
#'   fit. Defaults to TRUE, meaning that warnings will be issued by default.
#'
#' @return Invisibly returns a list containing diagnostic results:
#'   - `model_converged`: Logical indicating if the model has converged.
#'   - `k_prime`: The maximum available number of degrees of freedom that could
#'   be used in the GAM fit.
#'   - `k_edf`: Estimated degrees of freedom actually used by the smooth terms
#'   in the model.
#'   - `k_index`: The ratio of the residual variance of differenced
#'   near-neighbor residuals to the overall residual variance. This should be
#'   near 1 or above.
#'   - `k_p_value`: P-value for testing if k' is adequate for modeling the data.
#'   - `k_to_edf_ratio`: Ratio of k' to effective degrees of freedom of the
#'   smooth terms. k' should be well below the available edf.
#'   - `residual_autocorrelation`: Autocorrelation coefficients for residuals
#'   up to lag 7 or one-tenth of series length, whichever is smaller.
#'
#' @export
#' @seealso [mgcv::k.check] for a description of the diagnostic tests,
#'   [mgcv::choose.k] for a description of discussion of choosing the basis
#'   dimension, and Wood, Simon N. Generalized additive models: an introduction
#'   with R. chapman and hall/CRC, 2017. for a derivation of the metrics.
#' @examples
#' withr::with_seed(12345, {
#'   cases <- rpois(20, 10)
#' })
#' reference_date <- seq.Date(
#'   from = as.Date("2023-01-01"),
#'   length.out = 20,
#'   by = "day"
#' )
#' fit <- RtGam::RtGam(cases, reference_date)
#' check_diagnostics(fit)
check_diagnostics <- function(fit, warn_for_diagnostic_failure = TRUE) {
  diagnostics <- fit[["diagnostics"]]
  if (warn_for_diagnostic_failure) {
    issue_diagnostic_warnings(diagnostics)
  }
  invisible(diagnostics)
}

calculate_diagnostics <- function(fit) {
  converged <- fit$converged
  k_check <- mgcv::k.check(fit)
  max_lag <- min(7, round(nrow(fit$model) / 7))
  rho <- stats::acf(fit$residuals, plot = FALSE, lag.max = max_lag)[[1]][, , 1]

  is_smooth <- which(!is.na(k_check[, 4]))
  list(
    model_converged = converged,
    k_prime = max(k_check[is_smooth, 1]),
    k_edf = max(k_check[is_smooth, 2]),
    k_index = max(k_check[is_smooth, 3]),
    k_p_value = min(k_check[is_smooth, 4]),
    k_to_edf_ratio = max(k_check[is_smooth, 2] / k_check[is_smooth, 1]),
    residual_autocorrelation = rho[2:length(rho)]
  )
}

issue_diagnostic_warnings <- function(diagnostics) {
  if (!diagnostics[["model_converged"]][[1]]) {
    cli::cli_alert_danger(
      c("Model failed to converge. Inference is not reliable.")
    )
  }
  if (diagnostics[["k_to_edf_ratio"]] > 0.9) {
    cli::cli_bullets(c(
      "x" = "Effective degrees of freedom for one or more smooths near max",
      "!" = "Consider increasing {.arg k}",
      "*" = "Actual: {.val {round(diagnostics[['k_edf']], 3)}}",
      "*" = "Upper bound: {.val {diagnostics[['k_prime']]}}"
    ))
  }
  if (diagnostics[["k_p_value"]] < 0.05) {
    cli::cli_bullets(
      c(
        "!" = "k-index for one or more smooths is below 1",
        "*" = "k-index: {.val {round(diagnostics[['k_index']], 3)}}",
        "*" = "Associated p-value: {.val {round(diagnostics[['k_p_value']],
                                                   2)}}",
        "!" = "Suggests potential unmodeled residual trend.
                 Inspect model and/or consider increasing {.arg k}"
      )
    )
  }
  if (any(abs(diagnostics[["residual_autocorrelation"]]) > 0.5)) {
    cli::cli_bullets(c(
      "x" = "Residual autocorrelation present",
      "*" = "Rho: {.val {round(diagnostics[['residual_autocorrelation']],
                                 2)}}",
      "*" = "Inspect manually with {.code acf(residuals(fit$model))}",
      "!" = "Consider increasing {.arg k} and/or
               specifying {.arg rho} with {.arg backend} bam"
    ))
  }
  invisible(NULL)
}
