#' Fit the RtGam model with {mgcv}
#'
#' Use the pre-prepared model dataset and formula. Supply warnings as needed
fit_model <- function(data, formula, backend) {
  args <- args_constructor(data, formula, backend)
  call <- call_constructor(backend)

  do.call(
    call,
    args
  )
}

args_constructor <- function(data, formula, backend) {
  backend_agnostic_args <- list(
    formula = formula,
    data = data,
    # Negative binomial family with overdispersion param estimated
    family = "nb"
  )
  if (backend == "gam") {
    backend_specific_args <- list(
      method = "REML"
    )
  } else if (backend == "bam") {
    backend_specific_args <- list(
      method = "fREML",
      discrete = TRUE
    )
  } else {
    cli::cli_abort("Other backends not yet implemented")
  }

  c(backend_agnostic_args, backend_specific_args)
}

call_constructor <- function(backend) {
  # This is where we could implement {brms} or mgcv::ginla() at some point
  func <- paste0("mgcv::", backend)
  eval(parse(text = func))
}
