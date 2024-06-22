#' Fit a generalized additive model to incident cases
#'
#' Incident cases are modeled as a smooth function of time with a generalized
#' additive model (GAM).
#'
#' If more than three weeks of data are available, [RtGam] will fit a GAM with
#' an adaptive spline basis. This basis is so named because it allows the
#' wiggliness penalization to vary over time. If one part of the timeseries has
#' a sudden change in trend while another part shows a smooth increase, the
#' model can fit both components without smoothing away sharp changes or
#' introducing additional artificial wiggliness.
#'
#' The model introduces an additional penalty basis dimension for each
#' additional 21 days of observed data. A timeseries of 20 or fewer days would
#' have the same penalty the whole period, a timeseries of 21 to 42 days would
#' smoothly interpolate between two penalties, and so on for each additional 21
#' day period. This adaptive penalty increases the computational cost of the
#' model, but allows for a single model to adapt to changing epidemic dynamics.
#'
#' In the special case of 20 or fewer observed days, the model will use a single
#' penalty over the whole period and use a thin-plate spline as the smoothing
#' basis. The adaptive spline can only use a P-spline smoothing basis. The thin
#' plate spline generally has better performance and so [RtGam] uses it in this
#' special single-penalty case.
#'
#'
#' @param cases A vector of non-negative incident case counts occurring on an
#'   associated `reference_date`. Missing values (NAs) are not allowed.
#' @param reference_date The associated date on which the count of incident
#'   `cases` occurred. Missing dates are not allowed and dates can only occur
#'   once.
#' @param group The geographic grouping for the case/reference-date pair. Not
#'   yet implemented and a value other than `NULL` will throw an error.
#' @param k An integer, the _total_ dimension of all the smoothing basis
#'   functions. Defaults to `smooth_dim_heuristic(length(cases))`, which
#'   picks a reasonable estimate based on the number of provided data points.
#'   This total dimension is partitioned between the different smooths in the
#'   model. In the case of diagnostic issues in a fitted `RtGam` model,
#'   increasing the value of `k` above this default and refitting the model is a
#'   good first step. See the [smooth_dim_heuristic()] documentation for
#'   more information.
#' @param m An integer, the _total_ dimension of the penalty basis for the
#'
#' @seealso [smooth_dim_heuristic()] more information on the smoothing basis
#'   dimension and [mgcv::choose.k] for more general guidance on GAMs from
#'   `mgcv`
#' @return Stub function: NULL
#' @export
#' @examples
#' cases <- c(1, 2, 3)
#' reference_date <- as.Date(c("2023-01-01", "2023-01-02", "2023-01-03"))
#' mod <- RtGam::RtGam(cases, reference_date)
RtGam <- function(cases,
                  reference_date,
                  group = NULL,
                  k = smooth_dim_heuristic(length(cases))) {
  check_required_inputs_provided(
    cases,
    reference_date,
    group
  )
  validate(cases, reference_date, group)

  df <- prepare_inputs(cases, reference_date, group)
  formula <- formula_creator(
    n_timesteps = length(unique(df[["timesteps"]])),
    k = k,
    is_grouped = !rlang::is_null(group)
  )

  invisible(NULL)
}

#' Propose total smoothing basis dimension from number of data points
#'
#' Return a reasonable value for the `k` argument of [RtGam] (the _total_ smooth
#' basis dimension of the model's one or more smooth predictors) based on the
#' number of data points. The smooth basis dimension controls the maximum
#' degrees of freedom (and by proxy the "wiggliness") of the smooth predictors.
#' The estimation procedure leans toward providing an excess number of degrees
#' of freedom to the model. The consequence is slower model fits, but a better
#' chance of avoiding avoiding non-convergence due to undersmoothing. If
#' manually supplying a value to `k` rather than relying on the default
#' estimate, see *When to use a different value* for [RtGam]-specific
#' implementation guidance and [mgcv::choose.k] for more general debugging
#' guidance from the underlying model fitting package. Note that `k` may be a
#' minimum of 2 or a maximum of the number of data points.
#'
#' # How `k` is used
#'
#' The model is composed of one or more smooth predictors, depending the
#' specifics of the model specification. In a simple model with only one smooth
#' predictor, all the degrees of freedom from `k` would be applied to that
#' single smooth. In a more complex model composed of multiple smooth
#' predictors, the total degrees degrees of freedom made available by `k` would
#' be partitioned between the different smooths.
#'
#' # When to use a different value
#'
#' ## Model non-convergence
#'
#' When an [RtGam] model does not converge, a reasonable first debugging step is
#' to increase the value of `k` and refit the model. Commonly, GAMs exhibit
#' diagnostic issues when the model does not have enough flexibility to
#' represent the underlying data generating process. Increasing `k` above the
#' default estimate provides more flexibility.
#'
#' However, insufficient flexibility is not the only source of non-convergence.
#' When increasing `k` does not improve the default model diagnostics, manual
#' model checking via [mgcv::gam.check()] may be needed. Also see
#' [mgcv::choose.k] for guidance.
#'
#' ## Slow model fits
#'
#' [RtGam] models usually fit faster when the model has less flexibility (lower
#' values of `k`). The guess from [smooth_dim_heuristic()] leans toward
#' providing excess degrees of freedom, so model fits may take a little longer
#' than needed. If models are taking a long time to converge, it would be
#' reasonable to set `k` to a small value, checking for convergence, and
#' increasing `k` if needed until the model convergences. This approach may or
#' may not be faster than simply waiting for a model with a higher `k` to fit.
#'
#' ## Very wiggly data
#'
#' If running models in a setting where the data seem quite wiggly, exhibiting
#' sharp jumps or drops, a model with more flexibility than normal may be
#' needed. `k` should be increased to the maximum possible value. When running
#' pre-set models in production, it would also be reasonable to fix the value of
#' `k` above the default. Because GAMs penalize model wiggliness, the fit to
#' both wiggly and non-wiggly data is likely to be satisfactory, at the cost of
#' increased runtime.
#'
#' # Implementation details
#'
#' The algorithm to pick `k` is a piecewise function. When \eqn{n \le 10}, then
#' the returned value is \eqn{n}. When \eqn{n > 10}, then the returned value is
#' \eqn{ \lceil \sqrt{10n} \rceil }. This approach is loosely inspired by Ward
#' et al., 2021. As in Ward et al. the degrees of freedom of the spline (1) is
#' set to a reasonably high value to avoid oversmoothing and (2) scales with the
#' dimension of the data to accommodate changing trends over time.
#'
#' [smooth_dim_heuristic()] uses a piecewise function because each smooth
#' parameter needs its own degrees of freedom, which adds a fixed initial setup
#' cost. When the dimension of the data is small, the default value of `k`
#' increases linearly with the data to accommodate this fixed setup cost. When
#' the dimension of the data is larger, the default value of `k` increases with
#' the square root of the data to balance having sufficient basis dimension to
#' fit to changing trends over time without having so many dimensions that model
#' fits are very slow.
#'
#' @param n An integer, the dimension of the data.
#' @return An integer, the proposed _total_ smooth basis dimensionality
#'   available to the [RtGam] model.
#' @references Ward, Thomas, et al. "Growth, reproduction numbers and factors
#'   affecting the spread of SARS-CoV-2 novel variants of concern in the UK from
#'   October 2020 to July 2021: a modelling analysis." BMJ open 11.11 (2021):
#'   e056636.
#' @seealso [RtGam()] for the use-case and additional documentation as well as
#'   [mgcv::choose.k] and [mgcv::gam.check] for more general guidance from
#'   `mgcv`.
#' @export
#' @usage smooth_dim_heuristic(length(cases))
#' @examples
#' cases <- 1:10
#' k <- smooth_dim_heuristic(length(cases))
smooth_dim_heuristic <- function(n) {
  # Input checks
  rlang::check_required(n, "n", call = rlang::caller_env())
  check_vector(n)
  check_integer(n)
  check_no_missingness(n)
  check_elements_above_min(n, "n", min = 1)
  check_vector_length(length(n), "n", min = 1, max = 1)

  if (n < 10) {
    n
  } else {
    as.integer(ceiling(sqrt(10 * n)))
  }
}
