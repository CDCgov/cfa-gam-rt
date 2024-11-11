#' Validate user-provided inputs for type correctness
#'
#' Does not validate _sensibility_ of inputs nor does it do any
#' comparisons between the inputs to check lengths
#' @inheritParams RtGam
#' @param call Caller environment to be passed through the stack
#' @noRd
validate <- function(cases,
                     reference_date,
                     group,
                     k,
                     m,
                     call = rlang::caller_env()) {
  # Basic type checks
  validate_cases(cases, call)
  reference_date <- validate_dates(reference_date, "reference_date", call)
  validate_group(group, call)
  validate_min_dimensionality(k,
    arg = "k",
    min_dim = 3,
    max_val = length(cases),
    call
  )
  validate_min_dimensionality(m,
    arg = "m",
    min_dim = 1,
    max_val = length(unique(reference_date)),
    call = call
  )

  # Per-group checks
  check_vectors_equal_length(cases, reference_date, group, call)
  check_dates_unique(reference_date, group, call)

  invisible(reference_date)
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
  dates <- check_date(dates, arg, call)
  check_no_missingness(dates, arg, call)
  invisible(dates)
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
validate_min_dimensionality <- function(n, arg, min_dim, max_val = NA, call) {
  check_vector(n, arg, call = call)
  check_no_missingness(n, arg, call)
  check_integer(n, arg, call)
  check_elements_above_min(n, arg, min = min_dim, call = call)
  check_vector_length(length(n), arg, min = 1, max = 1, call = call)
  if (!rlang::is_na(max_val)) {
    check_elements_below_max(n, arg, max_val, call)
  }

  invisible()
}
