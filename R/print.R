#' Format the RtGam object for return from the main function/constructor
#' @noRd
format_for_return <- function(fit,
                              df,
                              group,
                              k,
                              m,
                              backend,
                              formula,
                              diagnostics) {
  formatted <- list(
    model = fit,
    data = df,
    min_date = min(df[["reference_date"]]),
    max_data = max(df[["reference_date"]]),
    k = k,
    m = m,
    backend = backend,
    formula = formula,
    diagnostics
  )

  structure(formatted, class = "RtGam")
}

#' Print an RtGam object
#'
#' @param x Fitted model object of class RtGam
#' @param ... further arguments to be passed to or from other methods. They are
#'   ignored in this function.
#'
#' @return The RtGam object, invisibly
#' @export
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
#' print(fit)
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
  cat(x$m, ")\n")
  cat("Specified maximum smoothing basis dimension: ", x$k, "\n")
  cat("Family:", x$model$family$family, "\n")
  cat("Link function:", x$model$family$link)


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
