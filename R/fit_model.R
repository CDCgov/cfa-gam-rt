fit_model <- function(data, formula, ...) {
  UseMethod("fit_model")
}

#' @export
fit_model.RtGam_gam <- function(
    data,
    formula,
    family = "nb",
    method = "REML",
    ...) {
  # Override the defaults in formals with the user-supplied args in dots
  mgcv::gam(
    formula = formula,
    family = family,
    data = data,
    method = method,
    ...
  )
}

#' @export
fit_model.RtGam_bam <- function(
    data,
    formula,
    family = "nb",
    method = "fREML",
    discrete = TRUE,
    ...) {
  mgcv::bam(
    formula = formula,
    family = family,
    data = data,
    method = method,
    discrete = discrete,
    ...
  )
}

#' Used to throw informative error if non-supported backend supplied
#' @export
#' @noRd
fit_model.default <- function(
    data,
    formula,
    ...) {
  requested_backend <- class(data)[1]
  all_backends <- methods(fit_model)
  # Drop fit_model.default
  supported_backends <- all_backends[!(all_backends == "fit_model.default")]

  cli::cli_abort(
    c("Requested {.field backend} {.val {requested_backend}} not supported",
      "!" = "Supported backends: {.val {supported_backends}}"
    ),
    class = "RtGam_invalid_input"
  )
}
