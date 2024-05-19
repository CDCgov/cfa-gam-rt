#' Validate user-provided inputs for type correctness
#'
#' Does not validate _sensibility_ of inputs nor does it do any
#' comparisons between the inputs to check lengths
#' @inheritParams RtGam
validate <- function(cases,
                     reference_date,
                     group,
                     call = rlang::caller_env()) {
  # Basic type checks
  validate_cases(cases, call)
  validate_dates(reference_date, "reference_date", call)
  validate_group(group, call)
  invisible()
}

validate_cases <- function(cases, call) {
  arg <- "cases"
  check_vector(cases, arg, call)
  check_integer(cases, arg, call)
  check_elements_non_neg(cases, arg, call)
  invisible()
}

#' Can be re-used for both reference and report dates
validate_dates <- function(dates, arg, call) {
  check_date(dates, arg, call)
  check_no_missingness(dates, arg, call)
  invisible()
}

validate_group <- function(group, call) {
  arg <- "group"
  if (!rlang::is_null(group)) {
    rlang::abort("Multiple groups not yet implemented",
      class = "RtGam_not_implemented"
    )
  }
}
