#' Fit a generalized additive model to incident cases
#'
#' # Model specification
#' Incident cases are modeled as a smooth function of time with generalized
#' additive models (GAMs). [RtGam] always fits a GAM, predicting incident
#' cases as a smooth trend of time. However, the model adapts the penalty on
#' wiggliness over time, allowing for changing epidemic dynamics.
#'
#' If more than three weeks of data are available, [RtGam] will fit a GAM with
#' an adaptive spline basis. This basis is so named because it allows the
#' wiggliness penalization to vary over time. Some parts of the fit can be more
#' or less wiggly than other parts. If one part of the timeseries has a sudden
#' change in trend while another part shows a smooth increase, the model can
#' fit both components without smoothing away sharp changes or introducing
#' additional artificial wiggliness.
#'
#' The model introduces an additional penalty basis dimension for each
#' additional 21 days of observed data. A timeseries of 20 or fewer days would
#' have the same penalty the whole period, a timeseries of 21 to 42 days would
#' smoothly interpolate between two penalties, and so on for each additional
#' 21 day period. This adaptive penalty increases the computational cost of the
#' model, but allows for a single model to adapt to changing epidemic dynamics.
#'
#' In the special case of 20 or fewer oberved days, the model will use a single
#' penalty over the whole period and use a thin-plate spline as the smoothing
#' basis. The adaptive spline can only use a P-spline smoothing basis. The thin
#' plate spline generally has better performance and so [RtGam] uses it in this
#' special single-penalty case.
#'
#' # Setting k
#' The argument `k` governs the _total_ basis dimension for the penalized
#' regression spline model used by `RtGam`. The model is composed of one or
#' more smooth predictors, depending the specifics of the model specification.
#' Each smooth predictor is penalized and has its own basis
#' dimension. The basis dimension controls the maximum degrees of
#' freedom (and by proxy the "wiggliness") of the smooth. [RtGam]'s `k`
#' argument controls the total degrees of freedom available to the different
#' smooth predictors. In a simple model with only one smooth predictor, all the
#' degrees of freedom from `k` would be applied to that single smooth. In a
#' more complex model composed of multiple smooth predictors, the total degrees
#' degrees of freedom made available by `k` would be partitioned between the
#' different smooths.
#'
#' In practice, GAMs penalize the wiggliness of smooth terms, so the fitted
#' model will use fewer effective degrees of freedom than the total available.
#' Although usually harmless to the model fit, excess degrees of freedom can
#' make models slower to fit. However, models with `k` set too low may
#' produce biased estimates or fail to converge.
#'
#' `RtGam` attempts to strike a reasonable balance between these concerns. It
#' uses a rule-of-thumb heuristic to set `k` based on the number of data points
#' provided. GAMs fit through [`mgcv`] (as in [RtGam]) usually fit much quicker
#' than MCMC-based approaches, so this diagnostic leans toward providing a
#' higher `k` than likely needed. This approach is a reasonable first pass, but
#' is not a substitute for the user's expert judgement. If the data exhibit a
#' sharp change in epidemic trend or the initial [RtGam] fit fails to converge,
#' it would be reasonable to fit the model with `k` higher than the default.
#' `k` can be set up to the number of data points, but not higher.
#'
#' @param cases A vector of non-negative incident case counts occurring on an
#'   associated `reference_date`. Missing values (NAs) are not allowed.
#' @param reference_date The associated date on which the count of incident
#'   `cases` occurred. Missing dates are not allowed and dates can only occur
#'   once.
#' @param group The geographic grouping for the case/reference-date pair. Not
#' yet implemented and a value other than `NULL` will throw an error.
#' @param k An integer, the _total_ dimension of all the smoothing basis
#'   functions. Defaults to `dimensionality_heuristic(length(cases))`, which
#'   picks a reasonable estimate based on the number of provided data points.
#'   This total dimension is partitioned between the different smooths in the
#'   model. In the case of diagnostic issues in a fitted `RtGam` model,
#'   increasing the value of `k` above this default and refitting the model is
#'   a good first step. See the `Setting k` section
#'   and [dimensionality_heuristic()] documentation for more information.
#'
#' @seealso [dimensionality_heuristic()] for the default basis dimension and
#'   [mgcv::choose.k] for more general guidance on GAMs from `mgcv`
#' @return Stub function: NULL
#' @export
#' @examples
#' cases <- c(1, 2, 3)
#' reference_date <- as.Date(c("2023-01-01", "2023-01-02", "2023-01-03"))
#' mod <- RtGam::RtGam(cases, reference_date)
RtGam <- function(cases,
                  reference_date,
                  group = NULL,
                  k = dimensionality_heuristic(length(cases))) {
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

#' Propose total basis dimensionality from number of data points
#'
#' Guess a reasonable value for the `k` argument of [RtGam] based on the number
#' of data points. This guess **may not work** and almost certainly is not the
#' optimal choice. Rather, it is a _reasonable_ first pass for many situations
#' and hopefully a good enough choice for most use cases. This guess leans
#' toward providing an excess number of degrees of freedom to the model. The
#' consequence is slower model fits, but a better chance of avoiding avoiding
#' non-convergence due to undersmoothing. See *When to use a different value*
#' for more guidance on use-cases where this heuristic is likely to fail and
#' alternative values may need to be chosen. Note that `k` may be a minimum of 2
#' or a maximum of the number of data points.
#'
#' # When to use a different value
#' ## Model non-convergence
#' When an [RtGam] model does not converge, a reasonable first debugging step
#' is to increase the value of `k` and refit the model. Commonly, GAMs exhibit
#' diagnostic issues when the model does not have enough flexibility to
#' represent the underlying data generating process. Increasing `k` above the
#' default heuristic guess provides more flexibility.
#'
#' However, insufficient flexibility is not the only source of non-convergence.
#' When increasing `k` does not improve the default model diagnostics, manual
#' model checking via [mgcv::gam.check()] may be needed. Also see
#' [mgcv::choose.k] for guidance.
#'
#' ## Slow model fits
#' [RtGam] models usually fit faster when the model has less flexibility (lower
#' values of `k`). The guess from [dimensionality_heuristic()] leans toward
#' providing excess degrees of freedom, so model fits may take a little longer
#' than needed. If models are taking a long time to converge, it would be
#' reasonable to set `k` to a small value, checking for convergence, and
#' increasing `k` if needed until the model convergences. This approach may or
#' may not be faster than simply waiting for a model with a higher `k` to fit.
#'
#' ## Very wiggly data
#' If running models in a setting where the data seem quite wiggly, exhibiting
#' sharp jumps or drops, a model with more flexibility than normal may be
#' needed. `k` should be increased to the maximum possible value. When running
#' pre-set models in production, it would also be reasonable to fix the value
#' of `k` above the default. Because GAMs penalize model wiggliness, the fit to
#' both wiggly and non-wiggly data is likely to be satisfactory, at the cost of
#' increased runtime.
#'
#' # Implementation details
#' The algorithm to pick `k` is a piecewise function. When \eqn{n \le 10}, then
#' the chosen value is \eqn{n}. When \eqn{n > 10}, then the selected value is
#' \eqn{ \lceil \sqrt{10n} \rceil }.
#' This approach is loosely inspired by Ward et al., 2021. As in Ward et al.,
#' the degrees of freedom of the spline is set to a reasonably high value to
#' avoid oversmoothing. The basis dimension increases with the length of the
#' timeseries. The scaled square root of the dimension of the data is used to
#' allow for the higher setup cost of the base model while still increasing the
#' available degrees of freedom when the length of the timeseries increases.
#'
#' @param n An integer, the dimension of the data.
#' @return An integer, the proposed _total_ basis dimensionality available to
#'   the [RtGam] model.
#' @references Ward, Thomas, et al. "Growth, reproduction numbers and factors
#'   affecting the spread of SARS-CoV-2 novel variants of concern in the UK from
#'   October 2020 to July 2021: a modelling analysis." BMJ open 11.11 (2021):
#'   e056636.
#' @seealso [RtGam()] for the use-case and additional documentation as well as
#'  [mgcv::choose.k] for more general guidance from `mgcv`.
#' @export
#' @examples
#' cases <- 1:10
#' k <- dimensionality_heuristic(length(cases))
dimensionality_heuristic <- function(n) {
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
