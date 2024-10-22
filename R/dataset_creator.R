#' Parse input vectors into a format for `{mgcv}`
#'
#' @inheritParams RtGam
#' @return A dataframe for mgcv
#' @export
#' @keywords internal
dataset_creator <- function(cases, reference_date, group, backend) {
  cases_int <- integerify_cases(cases)

  timestep <- dates_to_timesteps(
    reference_date,
    min_supplied_date = min(reference_date),
    max_supplied_date = max(reference_date)
  )

  if (rlang::is_null(group)) {
    group <- rep(NA, length(cases))
  }

  dat <- data.frame(
    cases = cases_int,
    timestep = timestep,
    reference_date = reference_date,
    group = group
  )

  class(dat) <- c(glue::glue("RtGam_{backend}"), class(dat))
  dat
}

#' Convert dates to an integer if needed
#'
#' @param cases The user-supplied cases vector
#' @return cases_int Cases verified to be an int
#' @noRd
integerify_cases <- function(cases) {
  if (!rlang::is_integer(cases)) {
    cli::cli_warn(c(
      "Coercing {.arg cases} to an integer vector",
      "i" = "{.arg cases} is a {.obj_type_friendly {cases}}",
      "x" = "RtGam uses a count model, requiring integer-valued cases"
    ))
    as.integer(cases)
  } else {
    cases
  }
}

#' Convert an arbitrary vector of dates to a vector of timesteps
#'
#' The `*_supplied_date` arguments are required rather than calculated
#' internally so that the function can be re-used in methods with a different
#' `reference_date` vector but the same min and max dates. This re-use ensures
#' that the timestep will match internally but the user can just supply dates
#' without knowledge of the internal timestep representation.
#'
#' Timesteps are scaled to be on \[0, 1\]. This small range is intentional --
#' I've seen it help numerical samplers converge and by default in R
#' recent dates are large integers.
#'
#' @inheritParams RtGam
#' @param min_supplied_date The minimum date supplied in `reference_date` to the
#'   class constructor.
#' @param max_supplied_date The maximum date supplied in `reference_date` to the
#'   class constructor.
#' @return A vector of timesteps, corresponding to the supplied reference dates
#' @export
#' @keywords internal
dates_to_timesteps <- function(reference_date,
                               min_supplied_date,
                               max_supplied_date) {
  ref_date_int <- as.integer(reference_date)
  min_int <- as.integer(min_supplied_date)
  max_int <- as.integer(max_supplied_date)

  (ref_date_int - min_int) / (max_int - min_int)
}
