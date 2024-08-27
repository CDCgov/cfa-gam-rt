#' Format the RtGam object for return from the main function/constructor
#'
#' @param fit The model fit created by [fit_model]
#' @param df The dataset created by [dataset_creator]
#'
#' @return An object of type RtGam
format_for_return <- function(fit, df, group, k, m, backend, formula) {
  formatted <- list(
    model = fit,
    data = df,
    min_date = min(df[["reference_date"]]),
    max_data = max(df[["reference_date"]]),
    k = k,
    m = m,
    backend = backend,
    formula = formula
  )

  structure(formatted, class = "RtGam")
}

#' Print an RtGam object
#'
#' @param x Fitted model object of class RtGam
#'
#' @return The RtGam object, invisibly
#' @export
print.RtGam <- function(x, ...) {
  cat("===============================\n")
  cat("Fitted RtGam model object (")
  cat(x$backend)
  cat(")\n")
  cat("===============================\n\n")

  cat("Model type: ")
  if (x$m > 1) {
    cat("Adaptive (m = ")
  } else {
    cat("Non-adaptive (m = ")
  }
  cat(x$m)
  cat(")\n")

  cat("Total smoothing basis dimension: ")
  cat(x$k)

  # TODO estimated edf & diagnostics

  cat("\n===============================\n")
  cat("\nObserved data points: ")
  cat(nrow(x$data))

  cat("\nDistinct reference dates: ")
  cat(length(unique(x[["data"]][["reference_date"]])))

  cat("\nDistinct groups: ")
  if (rlang::is_null(x[["data"]][["group"]][[1]])) {
    cat("1")
  } else {
    cat(length(unique(x[["data"]][["group"]])))
  }
  cat("\n\n")
  invisible(x)
}
