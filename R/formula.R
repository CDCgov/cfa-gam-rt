#' Build formula for `mgcv::gam()`
#'
#' Build up the formula as a string and and return a formula object meant for
#' use by [`mgcv::gam()`]. The formula components are built up based on the
#' parameters passed to [`RtGam`]. The model makes the assumptions that the
#' smoothness of epidemic dynamics does not change dramatically over a three
#' week period. For periods of three weeks or shorter, the model uses a simple
#' thin plate spline for the global epidemic trend. For periods of longer than
#' three days, the model uses an adaptive smoother with an additional penalty
#' basis for each additional three week period. The model smoothly interpolates
#' between the penalty bases and uses p-splines for the smoothing basis.
#'
#' Currently support for groups via hierarchical modeling  is not supported,
#' but when implemented it will use Model GS from Pederson et al., 2019.
#'
#' @param n_timesteps Number of distinct timesteps in the dataframe
#'  returned from [`prepare_inputs()`]
#' @param k Global basis dimension to be partitioned between the model smooths
#' @param is_grouped Whether to use a hierarchical model. Not yet supported.
#' @return A formula to be used by [`mgcv::gam()`]
#' @noRd
formula_creator <- function(n_timesteps, k, is_grouped) {
  outcome <- "cases"
  intercept <- "1"

  penalty_basis_dim <- penalty_basis_creator(n_timesteps)
  smooth_basis_dim <- smooth_basis_creator(k)

  # Apply adaptive spline if 3 weeks or more of data are available
  # nolint start
  if (n_timesteps >= 21) {
    # With adaptive basis, m refers to the order of the penalty matrix not the
    # order of the smoothing penalty as it does in the other smoothing bases.
    plus_global_trend <- glue::glue("+ s(timesteps,
                                         k = {smooth_basis_dim[['global_trend']]},
                                         m = {penalty_basis_dim},
                                         bs = 'ad')") # nolint
  } else {
    # Adaptive penalty with `m = 1` is equivalent to a non-adaptive smooth but
    # thin-plate performance is a bit better than p-spline, so preference to
    # fall back to thin-plate.
    plus_global_trend <- glue::glue("+ s(timesteps,
                                         k = {smooth_basis_dim[['global_trend']]},
                                         bs = 'tp')")
  }
  # nolint end

  f <- glue::glue("{outcome} ~ {intercept} {plus_global_trend}")
  stats::as.formula(f)
}

#' Create a penalty per three weeks of data for the global trend
#'
#' @inheritParams formula_creator
#' @return The penalty basis dimension for the global trend
#' @noRd
penalty_basis_creator <- function(n_timesteps) {
  as.integer(floor(n_timesteps / 21) + 1)
}

#' Partition global basis dimension into components
#'
#' @param k The global basis dimension
#' @return A list with named components matching formula components
#' @noRd
smooth_basis_creator <- function(k) {
  list(
    "global_trend" = k
  )
}
