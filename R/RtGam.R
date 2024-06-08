#' Stub: Will become fitting/class constructor
#'
#' Including as a stub function to showcase control flow
#'
#' ## Setting k
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
#' it would be reasonable to set `k` higher than the default and refit.
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
#'   a good first step. See the [Setting k] section
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

  invisible(NULL)
}
