#' Convert a Date to a YYYY-MM-DD string
#' @noRd
stringify_date <- function(d) {
  if (inherits(d, "Date")) {
    format(d, "%Y-%m-%d")
  } else {
    d
  }
}
#' Combine 2 vectors by alternating between elements
interleave <- function(v1, v2) {
  c(rbind(v1, v2))
}
