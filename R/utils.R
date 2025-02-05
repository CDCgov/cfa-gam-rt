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
#' @noRd
interleave <- function(v1, v2) {
  c(rbind(v1, v2))
}

#' Left-truncate and renormalize to a PMF
#'
#' The renewal equation assumes no same-day transmission. This assumption
#' means that when working with a generation interval formatted as a PMF
#' we need to remove the element corresponding to day zero. This function
#' drops the element and renormalizes the remaining density to sum to one.
#'
#' @param pmf A probability mass function formatted as a vector. Usually a
#'    generation interval indexed from day 0.
#' @param tol The numerical tolerance applied to the sum-to-one check on the
#'    renormalized PMF.
#' @return A probability mass function with no density in the first bin.
#' @export
truncate_and_renormalize <- function(pmf, tol = 1e-10) {
  pmf[1] <- 0
  outside_tolerance <- TRUE
  while (outside_tolerance) {
    pmf <- pmf / sum(pmf)
    outside_tolerance <- abs(sum(pmf) - 1) > tol
  }
  return(pmf)
}
