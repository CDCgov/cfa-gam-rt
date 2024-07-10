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
