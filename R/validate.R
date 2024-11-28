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
                     day_of_week,
                     k,
                     m,
                     call = rlang::caller_env()) {
  # Basic type checks
  validate_cases(cases, call)
  reference_date <- validate_dates(reference_date, "reference_date", call)
  validate_group(group, call)
  validate_day_of_week(day_of_week, call)
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
  check_vectors_equal_length(
    reference_date = reference_date,
    cases = cases,
    group = group,
    call = call
  )
  # Only check day of week length if it's not a bool
  if (length(day_of_week) > 1) {
    check_vectors_equal_length(
      reference_date = reference_date,
      day_of_week = day_of_week,
      call = call
    )
  }
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

#' Check day of week is a bare bool or is vector of corresponding elements
validate_day_of_week <- function(day_of_week, call) {
  arg <- "day_of_week"
  # If `day_of_week` is a bare bool, pass
  if (!(rlang::is_true(day_of_week) || rlang::is_false(day_of_week))) {
    check_vector(day_of_week, arg, call)
    check_no_missingness(day_of_week, arg, call)
  }
  invisible()
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

#' Check user-provided input matches expectations
#' @noRd
validate_predict_inputs <- function(
    parameter,
    mean_delay,
    gi_pmf,
    call = rlang::caller_env()) {
  rlang::arg_match(parameter,
    values = c(
      "obs_cases",
      "r",
      "Rt"
    ),
    error_call = call
  )
  if (parameter == "obs_cases") {
    if (!rlang::is_null(mean_delay)) {
      cli::cli_alert(
        "{.arg mean_delay} ignored when {.arg parameter} is {.val obs_cases}"
      )
    }
    if (!rlang::is_null(gi_pmf)) {
      cli::cli_alert(
        "{.arg gi_pmf} ignored when {.arg parameter} is {.val obs_cases}"
      )
    }
  } else {
    if (rlang::is_null(mean_delay)) {
      cli::cli_abort(
        "{.arg parameter} {.val {parameter}} requires {.arg mean_delay}",
        call = call
      )
    }
    check_integer(mean_delay, "gi_pmf", call = call)
  }
  if (parameter == "Rt") {
    if (rlang::is_null(gi_pmf)) {
      cli::cli_abort(
        c(
          "{.arg gi_pmf} is required when",
          "{.arg parameter} is {.val Rt}"
        ),
        call = call
      )
    }
    check_vector(gi_pmf, "gi_pmf", call = call)
    check_no_missingness(gi_pmf, "gi_pmf", call = call)
    check_elements_above_min(gi_pmf, "gi_pmf", 0, call = call)
    check_elements_below_max(gi_pmf, "gi_pmf", 1, call = call)
    check_sums_to_one(gi_pmf, "gi_pmf", call = call)
  }
}
