fit_model <- function(data, formula, user_supplied_args) {
  UseMethod("fit_model")
}

#' @export
fit_model.RtGam_gam <- function(data, formula, user_supplied_args) {
  default_args <- list(
    formula = formula,
    data = data,
    family = "nb",
    method = "REML"
  )
  args <- utils::modifyList(default_args, user_supplied_args)

  do.call(mgcv::gam, args)
}

#' @export
fit_model.RtGam_bam <- function(data, formula, user_supplied_args) {
  default_args <- list(
    formula = formula,
    data = data,
    family = "nb",
    method = "fREML",
    discrete = TRUE
  )
  args <- utils::modifyList(default_args, user_supplied_args)

  do.call(mgcv::bam, args)
}
