#' Check diagnostics from a fitted model
#'
#' @param fit A fitted `RtGam` model object
#' @param warn_for_diagnostic_failure A bool, whether to issue warnings for
#'   potential diagnostic issues.
#'
#' @return Invisibly, a list of diagnostics. This diagnostic list is also
#'   present in the model object under `diagnostics`.
#' @export
#'
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
  diagnostics <- calculate_diagnostics(fit[["model"]])
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

  list(
    model_converged = converged,
    k_edf = k_check[2],
    k_index = k_check[3],
    k_p_value = k_check[4],
    k_to_edf_ratio = k_check[2] / k_check[1],
    residual_autocorrelation = rho[2:length(rho)]
  )
}

issue_diagnostic_warnings <- function(diagnostics) {
  if (!diagnostics[["model_converged"]]) {
    cli::cli_alert_danger(
      c("Model failed to converge. Inference is not reliable.")
    )
  }
  if (diagnostics[["k_to_edf_ratio"]] > 0.9) {
    cli::cli_bullets(c(
      "x" = "Effective degrees of freedom is near the supplied upper bound",
      "!" = "Consider increasing {.arg k}",
      "*" = "Actual: {.val {round(diagnostics[['k_edf']], 3)}}",
      "*" = "Upper bound: {.val {round(diagnostics[['k\\'']], 3)}}"
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
