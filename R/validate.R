#' Validate user-provided inputs for type correctness
#'
#' Does not validate _sensibility_ of inputs nor does it do any
#' comparisons between the inputs to check lengths
#' @inheritParams RtGam
#' @param call Caller environment to be passed through the stack
validate <- function(cases,
                     reference_date,
                     group,
                     k,
                     m,
                     call = rlang::caller_env()) {
  # Basic type checks
  validate_cases(cases, call)
  validate_dates(reference_date, "reference_date", call)
  validate_group(group, call)
  validate_min_dimensionality(k, "k", min_dim = 3, call)
  validate_min_dimensionality(m, "m", min_dim = 1, call)

  # Per-group checks
  check_vectors_equal_length(cases, reference_date, group, call)
  check_dates_unique(reference_date, group, call)

  invisible()
}

validate_cases <- function(cases, call) {
  arg <- "cases"
  check_vector(cases, arg, call)
  check_no_missingness(cases, arg, call)
  check_integer(cases, arg, call)
  check_elements_above_min(cases, arg, min = 0, call = call)
  invisible()
}

validate_dates <- function(dates, arg, call) {
  arg <- "dates"
  # No vector check because date vector is of type Date
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

#' Used by both dimensionality_heuristic() and RtGam()
#' @noRd
validate_min_dimensionality <- function(n, arg, min_dim, call) {
  check_vector(n, arg, call = call)
  check_no_missingness(n, arg, call)
  check_integer(n, arg, call)
  check_elements_above_min(n, arg, min = min_dim, call = call)
  check_vector_length(length(n), arg, min = 1, max = 1, call = call)

  invisible()
}
