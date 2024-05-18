#' Stub: Will become fitting/class constructor
#'
#' Including as a stub function to showcase control flow
#' @param cases
#' @param reference_date
#' @param group
#'
#' @returns Stub function: NULL
#' @export
#' @examples
#' cases <- c(1, 2, 3)
#' reference_date <- as.Date(c("2023-01-01", "2023-01-02", "2023-01-03"))
#' mod <- RtGam::fit(cases, reference_date)
fit <- function(cases, reference_date, group = NULL) {
  check_required_inputs_provided(cases, reference_date, group)
}
