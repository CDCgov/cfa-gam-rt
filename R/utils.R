#' Convert a Date to a YYYY-MM-DD string
#' @noRd
stringify_date <- function(d) {
  if (inherits(d, "Date")) {
    format(d, "%Y-%m-%d")
  } else {
    d
  }
}
