#' Build formula for model fitting backend
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
#' @param m Penalty basis dimension on the global smooth
#' @param is_grouped Whether to use a hierarchical model. Not yet supported.
#' @return A formula to be used by [`mgcv::gam()`]
#' @noRd
formula_creator <- function(k, m, is_grouped, day_of_week) {
  outcome <- "cases"
  intercept <- "1"

  smooth_basis_dim <- smooth_basis_creator(k)

  # nolint start
  if (m > 1) {
    # With adaptive basis, m refers to the order of the penalty matrix not the
    # order of the smoothing penalty as it does in the other smoothing bases.
    plus_global_trend <- glue::glue("+ s(timestep,
                                         k = {smooth_basis_dim[['global_trend']]},
                                         m = {m},
                                         bs = 'ad')") # nolint
  } else {
    # Adaptive penalty with `m = 1` is equivalent to a non-adaptive smooth but
    # thin-plate performance is a bit better than p-spline, so preference to
    # fall back to thin-plate.
    plus_global_trend <- glue::glue("+ s(timestep,
                                         k = {smooth_basis_dim[['global_trend']]},
                                         bs = 'tp')")
  }

  if (is.factor(day_of_week)) {
    plus_day_of_week <- "+ s(day_of_week, bs = 're')"
  } else {
    plus_day_of_week <- ""
  }

  f <- glue::glue("{outcome} ~ {intercept} {plus_global_trend} {plus_day_of_week}")
  # nolint end
  stats::as.formula(f)
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

#' Issue warnings if parameterization allowed but suboptimal
#'
#' @noRd
warn_for_suboptimal_params <- function(data, m, k) {
  n_unique_date <- length(unique(data[["timepoint"]]))
  total_dim <- nrow(data)

  # From mgcv: "Bear in mind that adaptive smoothing places quite severe demands
  # on the data. For example, setting ‘m=10’ for a univariate smooth of 200 data
  # is rather like estimating 10 smoothing parameters, each from a data series
  # of length 20. The problem is particularly serious for smooths of 2
  # variables, where the number of smoothing parameters required to get
  # reasonable flexibility in the penalty can grow rather fast, but it often
  # requires a very large smoothing basis dimension to make good use of this
  # flexibility. In short, adaptive smooths should be used sparingly and with
  # care."
  if (m / n_unique_date > 0.2) {
    cli::cli_warn(
      c("Using {m} penalty bases with {n_unique_date} dates supplied",
        "Consider decreasing penalty dimension {.arg m}",
        "i" = "See {.func penalty_dim_heuristic()} for guidance"
      )
    )
  }

  invisible()
}
