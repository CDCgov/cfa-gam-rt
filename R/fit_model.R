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
  formal_arg_names <- Filter(function(x) x != "...", names(formals()))
  formals <- as.list(environment())[formal_arg_names]
  dots <- rlang::list2(...)
  args <- utils::modifyList(formals, dots)

  do.call(mgcv::gam, args)
}

#' @export
fit_model.RtGam_bam <- function(
    data,
    formula,
    family = "nb",
    method = "fREML",
    discrete = TRUE,
    ...) {
  # Override the defaults in formals with the user-supplied args in dots
  formal_arg_names <- Filter(function(x) x != "...", names(formals()))
  formals <- as.list(environment())[formal_arg_names]
  dots <- rlang::list2(...)
  args <- utils::modifyList(formals, dots)
  do.call(mgcv::bam, args)
}

#' Used to throw informative error if non-supported backend supplied
#' @export
fit_model.default <- function(
    data,
    formula,
    ...) {
  requested_backend <- class(data)[1]
  supported_backends <- c("gam", "bam")

  cli::cli_abort(
    c("Requested {.field backend} {.val {requested_backend}} not supported",
      "!" = "Allowed backends: {.val {supported_backends}}"
    ),
    class = "RtGam_invalid_input"
  )
}
