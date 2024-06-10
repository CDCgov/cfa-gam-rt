#' Stub: Will become fitting/class constructor
#'
#' Including as a stub function to showcase control flow
#' @param cases A vector of non-negative incident case counts occurring on an
#'   associated `reference_date`. Missing values (NAs) are not allowed.
#' @param reference_date The associated date on which the count of incident
#'   `cases` occurred. Missing dates are not allowed and dates can only occur
#'   once.
#' @param group The geographic grouping for the case/reference-date pair. Not
#' yet implemented and a value other than `NULL` will throw an error.
#'
#' @returns Stub function: NULL
#' @export
#' @examples
#' cases <- c(1, 2, 3)
#' reference_date <- as.Date(c("2023-01-01", "2023-01-02", "2023-01-03"))
#' mod <- RtGam::RtGam(cases, reference_date)
RtGam <- function(cases,
                  reference_date,
                  group = NULL) {
  check_required_inputs_provided(
    cases,
    reference_date,
    group
  )
  validate(cases, reference_date, group)

  df <- prepare_inputs(cases, reference_date, group)

  invisible(NULL)
}
