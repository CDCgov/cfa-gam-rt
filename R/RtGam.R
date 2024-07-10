#' Fit a generalized additive model to incident cases
#'
#' Incident cases are modeled as a smooth function of time with a generalized
#' additive model (GAM). The model is fit with [mgcv::gam()] and some
#' familiarity with `mgcv` may be helpful.
#'
#' # Model specification
#'
#' Incident cases (\eqn{y}) are modeled as smoothly changing over time:
#'
#' \deqn{\text{log}\{E(y)\} = \alpha + f_{\text{global}(t)}}
#'
#' where incidence is negative-binomially distributed and \eqn{f(t)} is a smooth
#' function of time.
#'
#' @param cases A vector of non-negative incident case counts occurring on an
#'   associated `reference_date`. Missing values (NAs) are not allowed.
#' @param reference_date The associated date on which the count of incident
#'   `cases` occurred. Missing dates are not allowed and dates can only occur
#'   once.
#' @param group The grouping variable for the case/reference-date pair. Not yet
#'   implemented and a value other than `NULL` will throw an error.
#' @param k An integer, the _total_ dimension of all the smoothing basis
#'   functions. Defaults to `smooth_dim_heuristic(length(cases))`, which picks a
#'   reasonable estimate based on the number of provided data points. This total
#'   dimension is partitioned between the different smooths in the model. In the
#'   case of diagnostic issues in a fitted `RtGam` model, increasing the value
#'   of `k` above this default and refitting the model is a good first step. See
#'   the [smooth_dim_heuristic()] documentation for more information.
#' @param m An integer, the dimension of the penalty basis for the global smooth
#'   trend. If `m` is greater than 1, the smooth's wiggliness can change over
#'   time. An increase in this value above the default should be done carefully.
#'   See [penalty_dim_heuristic()] for more information on `m` and when to
#'   consider changing the default.
#' @param backend One of `gam` or `bam`; defaults to `gam`. In general, models
#'   should be fit with [mgcv::gam()]. If [mgcv::gam()] is too slow,
#'   [mgcv::bam()] converges more quickly but introduces some additional
#'   numerical error. Note that the `bam` backend uses the `discrete = TRUE`
#'   option for an additional speedup. See [mgcv::bam()] for more information.
#' @param ... Additional arguments passed to the specified modelling backend.
#'   For example, the default negative binomial error structure could be changed
#'   to poisson in the default [mgcv::gam] backend by passing `family =
#'   "poisson"`.
#' @seealso [smooth_dim_heuristic()] more information on the smoothing basis
#'   dimension, [mgcv::choose.k] for more general guidance on GAMs from `mgcv`,
#'   and [mgcv::gam]/[mgcv::bam] for documentation on arguments to the model
#'   fitting functions.
#' @return Stub function: NULL
#' @export
#' @examples
#' cases <- c(1, 2, 3)
#' reference_date <- as.Date(c("2023-01-01", "2023-01-02", "2023-01-03"))
#' mod <- RtGam::RtGam(cases, reference_date)
RtGam <- function(cases,
                  reference_date,
                  group = NULL,
                  k = smooth_dim_heuristic(length(cases)),
                  m = penalty_dim_heuristic(length(unique(reference_date))),
                  backend = "gam",
                  ...) {
  check_required_inputs_provided(
    cases,
    reference_date,
    group,
    k,
    m,
    backend
  )
  validate(cases, reference_date, group, k, m)

  df <- dataset_creator(cases, reference_date, group, backend)
  formula <- formula_creator(
    k = k,
    m = m,
    is_grouped = !rlang::is_null(group)
  )

  fit <- fit_model(df, formula, ...)

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

#' Propose a penalty basis dimension based on the number of observed dates
#'
#' Return a reasonable value for the `m` argument of [RtGam()] based on the
#' number of dates that cases are observed. The `m` argument controls the
#' dimension of the smoothing penalty basis for the model's global smooth trend
#' (see the *Model specification* section of the [RtGam()] documentation for
#' more information about the global trend). The penalty basis dimension
#' controls how much the wiggliness of the global smooth trend can vary over
#' time. Higher values of `m` help the model to adapt quickly to different
#' epidemic regimes, but are computationally costly.
#'
#' # How `m` is used
#'
#' The parameter `m` controls the penalty basis dimension of the model's global
#' smooth trend. If `m` is 1, there will be single constant penalty on
#' wiggliness over the entire smooth and [RtGam] will use a thin-plate spline
#' basis for its superior performance in single-penalty settings. If `m` is 2 or
#' more, the model will use `m` distinct penalties on the smooth trend's
#' wiggliness and use an adaptive spline basis. The realized penalty at each
#' timepoint smoothly interpolates between the `m` estimated wiggliness
#' penalties. This adaptive penalty increases the computational cost of the
#' model, but allows for a single model to adapt to changing epidemic dynamics
#' without oversmoothing or introducing spurious wiggly trends.
#'
#' # When to use a different value
#'
#' ## Very slow
#'
#' Decreasing the penalty basis dimension makes the model less demanding to fit.
#' `mgcv` describes an adaptive penalty with 10 basis dimensions and 200 data
#' points as roughly equivalent to fitting 10 GAMs each from 20 data points.
#' Using a single penalty throughout the model is much simpler than using an
#' adaptive smooth and should be preferred where possible. See
#' `[mgcv::smooth.construct.ad.smooth.spec]` for more information on how the
#' adaptive smooth basis uses the penalty dimension.
#'
#' ## Observed over-smoothing of non-stationary data
#'
#' If a fitted model is observably over-smoothing, it may be reasonable to refit
#' with a higher penalty basis dimension. Moments with a sudden change in
#' epidemic dynamics, such as a sharp epidemic peak, can be challenging to fit
#' with smooth functions. This option should be used with care due to the
#' increased computational cost.
#'
#' # Implementation details
#'
#' The algorithm to pick `m` is \eqn{\lfloor \frac{n}{21} \rfloor + 1} where
#' \eqn{n \in \mathbb{W}} is the number of observed dates. This algorithm
#' assumes that over a 21-day period, epidemic dynamics remain roughly similarly
#' wiggly. Sharp jumps or drops requiring a very wiggly trend would remain
#' similarly plausible over much of the 21-day band.
#'
#' @param n An integer, the number of dates with an associated case observation.
#' @return An integer, the proposed penalty basis dimension to be used by the
#'   global trend.
#' @seealso [RtGam()] for the use-case and additional documentation as well as
#'   [mgcv::smooth.construct.ad.smooth.spec] for an explanation of the
#'   underlying adaptive-smooth machinery.
#' @export
#' @examples
#' # Default use invokes `unique()` in case of repeated dates from groups
#' reference_date <- as.Date(c("2023-01-01", "2023-01-02", "2023-01-03"))
#' m <- penalty_dim_heuristic(length(reference_date))
#'
penalty_dim_heuristic <- function(n) {
  # Input checks
  rlang::check_required(n, "n", call = rlang::caller_env())
  check_vector(n)
  check_integer(n)
  check_no_missingness(n)
  check_elements_above_min(n, "n", min = 1)
  check_vector_length(length(n), "n", min = 1, max = 1)

  as.integer(floor(n / 21) + 1)
}
