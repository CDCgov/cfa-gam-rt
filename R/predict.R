#' Draw posterior samples from a fitted RtGam model
#'
#' Generate posterior draws from an `RtGam` fit. Prediction dates can be
#' specified flexibly using various approaches, and predictions can be drawn for
#' different model parameters.
#'
#' @param object An `RtGam` object created using the [RtGam()]
#' function.
#' @param parameter A character string specifying the prediction target.
#'   Options are `"obs_cases"` (observed cases), `"r"` (growth rate), or
#'   `"Rt"` (reproduction number). Default is `"obs_cases"`.
#' @param horizon Optional. An integer indicating the number of days to
#'   forecast beyond the last date in the model fit. For example, `horizon = 7`
#'   predicts the next 7 days.
#' @param min_date,max_date Optional. Date-like objects specifying the start
#'   and end of the prediction range. See **Details** for more information on
#'   their usage.
#' @param n An integer specifying the number of posterior samples to use
#'   for predictions. Default is 100.
#' @param mean_delay Optional. An integer specifying the mean number of days
#'   between an individual becoming infected and their case being observed
#'   (e.g., through an emergency department visit or hospitalization). This
#'   value shifts the predictions to account for reporting delays. It is
#'   required when predicting `"r"` (growth rate) or `"Rt"` (reproduction
#'   number).
#' @param gi_pmf Optional. A numeric vector specifying the generation interval
#'   probability mass function (PMF), required when `parameter = "Rt"`. The PMF
#'   must be a proper probability distribution (summing to one) with the first
#'   element set to zero to exclude same-day transmission, as required by the
#'   renewal equation. For more information and tools to handle delay
#'   distributions, see the
#'   [primarycensored](https://CRAN.R-project.org/package=primarycensored)
#'   package.
#' @param seed An integer specifying the random seed for reproducibility.
#'   Default is 12345.
#' @param ... Additional arguments passed to the underlying sampling functions:
#'   - When `parameter = "obs_cases"`, arguments are passed to `gratia:
#' posterior_samples`.
#'   - When `parameter = "r"` or `"Rt"`, arguments are passed to `gratia:
#' fitted_samples`.
#'
#' @details
#' Prediction dates can be defined in four ways:
#'
#' 1. **Default Date Range**: By using only the fit object, predictions are made
#'    across the full date range in the original model.
#' 2. **Using `horizon`**: Extends predictions up to `horizon` days beyond the
#'    last date in the model fit.
#' 3. **Using `min_date` and `horizon`**: Predictions start from `min_date` and
#'    extend up to `horizon` days after the fit’s last date.
#' 4. **Using `min_date` and `max_date`**: Generates predictions for all dates
#'    within this specified range, inclusive.
#'
#' The `mean_delay` parameter adjusts predictions for the
#' temporal lag between infection and case observation. For example, if
#' `mean_delay = 5`,
#' the model assumes that observed cases reflect infections that occurred on
#' average five days
#' earlier. This adjustment ensures that estimates of growth rates (`"r"`) and
#' reproduction
#' numbers (`"Rt"`) align with the correct underlying temporal dynamics.
#'
#' The `parameter` argument determines the type of predictions:
#' - `"obs_cases"`: Observed cases, including uncertainty from the model's
#' fit.
#' - `"r"`: Growth rate, calculated using the centered difference between time
#' steps.
#' - `"Rt"`: Reproduction number, incorporating delay distributions and
#' convolution.
#'
#' Samples are drawn from the posterior distribution of the fitted model using
#' the `gratia` package. The model estimates basis function coefficients on the
#' smooth terms \eqn{\hat \beta} and smoothing parameter(s) \eqn{\lambda}. The
#' has posterior distribution
#' \deqn{\beta | \lambda \sim N(\hat \beta, \mathbf{V}_{\hat \beta}}
#' where \eqn{\mathbf{V}_{\hat \beta}} is the smoothing-parameter uncertainty
#' corrected covariance matrix of the basis function coefficients. We draw
#' samples from \eqn{\mathbf{V}_{\hat \beta}} and multiply them by the dates of
#' interest to generate posterior estimates.
#'
#' For the intrinsic growth rate, we draw one day before and one day after every
#' day of interest. We difference these two days within the smooth to get and
#' divide by two to generate the discrete centered derivative.
#'
#' For the Rt we map the estimated values back to the response scale with the
#' inverse link function (\eqn{I}) and use the generation interval probability
#' mass function (\eqn{w}) to estimate Rt via the Cori method:
#' \deqn{I_t / \sum_{s = 1}^t{I_{t - s} w_s}}
#'
#' For observed incident cases, we apply observation error to the posterior
#' expected incidence to generate posterior predicted incidence.
#'
#' @seealso [gratia::fitted_samples()], [gratia::posterior_samples()]
#' @references Miller, David L. "Bayesian views of generalized additive
#'    modelling." arXiv preprint arXiv:1902.01330 (2019).
#' Gostic, Katelyn M., et al. "Practical considerations for measuring the
#'   effective reproductive number, R t." PLoS computational biology 16.12
#'   (2020): e1008409.
#' Simpson, Gavin L. "Gratia: An R package for exploring generalized additive
#'   models." arXiv preprint arXiv:2406.19082 (2024).
#' Cori A, Ferguson NM, Fraser C, Cauchemez S. A New Framework and Software to
#'  Estimate Time-Varying Reproduction Numbers During Epidemics. Am J Epidemiol.
#'    2013;178(9):1505–12. pmid:24043437
#' @return
#' A data frame in
#' [tidy format](https://www.jstatsoft.org/article/view/v059i10),
#' where each row represents a posterior draw for a specific date, with the
#' following columns:
#'
#' - `reference_date`: The prediction date.
#' - `.response`: The predicted value for the target parameter.
#' - `.draw`: The index of the posterior draw.
#'
#' Example output:
#' ```
#'   reference_date .response .draw
#' 1     2023-01-01        18     1
#' 2     2023-01-02        13     1
#' 3     2023-01-03        21     1
#' 4     2023-01-01        11     2
#' 5     2023-01-02        19     2
#' 6     2023-01-03        24     2
#' ```
#'
#' @export
predict.RtGam <- function(
    object,
    parameter = "obs_cases",
    horizon = NULL,
    min_date = NULL,
    max_date = NULL,
    n = 100,
    mean_delay = NULL,
    gi_pmf = NULL,
    seed = 12345,
    ...) {
  validate_predict_inputs(
    parameter,
    mean_delay,
    gi_pmf
  )
  if (parameter == "obs_cases") {
    predict_obs_cases(
      object = object,
      horizon = horizon,
      min_date = min_date,
      max_date = max_date,
      n = n,
      mean_delay = mean_delay,
      gi_pmf = gi_pmf,
      seed = seed,
      ...
    )
  } else if (parameter == "r") {
    predict_growth_rate(
      object = object,
      horizon = horizon,
      min_date = min_date,
      max_date = max_date,
      n = n,
      mean_delay = mean_delay,
      seed = seed,
      ...
    )
  } else if (parameter == "Rt") {
    predict_rt(
      object = object,
      horizon = horizon,
      min_date = min_date,
      max_date = max_date,
      n = n,
      mean_delay = mean_delay,
      seed = seed,
      gi_pmf = gi_pmf,
      ...
    )
  }
}

#' Parameter-specific posterior predictions
#'
#' @inherit predict.RtGam
#' @param call The calling environment to pass to error messages
#' @name predictor
#' @keywords internal
NULL

#' @rdname predictor
# " @export
predict_obs_cases <- function(
    object,
    horizon = NULL,
    min_date = NULL,
    max_date = NULL,
    n = 10,
    gi_pmf = NULL,
    seed = 12345,
    call = rlang::caller_env(),
    ...) {
  newdata <- create_newdata_dataframe(
    object = object,
    parameter = "obs_cases",
    min_date = min_date,
    max_date = max_date,
    horizon = horizon,
    call = call
  )

  # Use `posterior_samples()` over `fitted_samples()` to get response
  # w/ obs uncertainty
  fitted <- gratia::posterior_samples(
    object[["model"]],
    data = newdata,
    unconditional = TRUE,
    n = n,
    seed = seed,
    ...
  )
  format_predicted_dataframe(fitted, newdata)
}

#' @rdname predictor
# " @export
predict_growth_rate <- function(
    object,
    horizon = NULL,
    min_date = NULL,
    max_date = NULL,
    n = 10,
    gi_pmf = NULL,
    seed = 12345,
    mean_delay,
    call = rlang::caller_env(),
    ...) {
  delta <- compute_delta(object)
  newdata <- create_newdata_dataframe(
    object = object,
    parameter = "r",
    mean_delay = mean_delay,
    min_date = min_date,
    max_date = max_date,
    horizon = horizon,
    delta = delta,
    call = call
  )
  # Amend the returned newdata row IDs to make paired timmestep groups
  # transparent. We have 1, 2, 3, ... and map to 1, NA, 2, NA, ...
  # to show that the timesteps are (t0, t2), (t1, t3), ...
  newdata[".row"] <- interleave(seq(1, nrow(newdata) / 2, 1), NA)
  fitted <- gratia::fitted_samples(
    object[["model"]],
    data = newdata,
    n = n,
    seed = seed,
    unconditional = TRUE,
    scale = "linear_predictor",
    ...
  )
  timestep_first_row <- which((fitted[[".row"]] - 1) %% 2 == 0)
  fitted <- data.frame(
    .row = (fitted[timestep_first_row, ".row"] + 1) / 2,
    .response = discrete_diff_derivative(fitted[[".fitted"]]),
    .draw = fitted[timestep_first_row, ".draw"]
  )
  format_predicted_dataframe(
    fitted = fitted,
    newdata = newdata
  )
}

#' @rdname predictor
#' @export
predict_rt <- function(
    object,
    horizon = NULL,
    min_date = NULL,
    max_date = NULL,
    n = 10,
    gi_pmf = NULL,
    seed = 12345,
    mean_delay,
    call = rlang::caller_env(),
    ...) {
  newdata <- create_newdata_dataframe(
    object = object,
    parameter = "Rt",
    mean_delay = mean_delay,
    min_date = min_date,
    max_date = max_date,
    horizon = horizon,
    gi_pmf = gi_pmf,
    call = call
  )
  fitted <- gratia::fitted_samples(
    object[["model"]],
    data = newdata,
    n = n,
    seed = seed,
    unconditional = TRUE,
    scale = "response",
    ...
  )
  # Rt calculation
  fitted <- rt_by_group(fitted,
    group_cols = ".draw",
    value_col = ".fitted",
    vec = gi_pmf
  )
  format_predicted_dataframe(
    fitted = fitted,
    newdata = newdata
  )
}

#' Convert from user specification to necessary date range
#'
#' @inheritParams predict.RtGam
#' @param call The calling environment to be reflected in the error message
#'
#' @return List with two elements: min_date and max_date
#' @keywords internal
#' @importFrom rlang %||%
parse_predict_dates <- function(
    object,
    min_date = NULL,
    max_date = NULL,
    horizon = NULL,
    call = rlang::caller_env()) {
  if (!rlang::is_null(min_date)) {
    min_date <- check_date(min_date, call = call)
  }
  if (!rlang::is_null(max_date)) {
    max_date <- check_date(max_date, call = call)
  }
  if (!rlang::is_null(horizon)) check_integer(horizon, call = call)

  # Handle horizon to estimate dates if provided
  if (!rlang::is_null(horizon)) {
    if (!rlang::is_null(max_date)) {
      cli::cli_abort("Cannot specify both {.arg horizon} and {.arg max_date}",
        call = call
      )
    }
    min_date <- min_date %||% (object[["max_date"]] + 1)
    max_date <- object[["max_date"]] + horizon
  } else {
    # Default to object's date range if not specified
    min_date <- min_date %||% object[["min_date"]]
    max_date <- max_date %||% object[["max_date"]]
  }

  # Ensure min_date is before max_date
  if (min_date >= max_date) {
    cli::cli_alert_warning("Swapping {.arg min_date} and {.arg max_date}")
    cli::cli_alert(c(
      "{.arg min_date} {.val {min_date}} ",
      "is after {.arg max_date} {.val {max_date}}"
    ))
    temp_var <- max_date
    max_date <- min_date
    min_date <- temp_var
  }

  seq.Date(
    from = min_date,
    to = max_date,
    by = "day"
  )
}

#' Map user-req'd dates to model-applied dates. Incidence is a
#' mean-shift ahead of cases and Rt requires GI padding for the
#' convolution
#' @noRd
shift_desired_dates <- function(
    parameter,
    desired_min_date,
    desired_max_date,
    mean_delay,
    gi_pmf) {
  if (parameter == "obs_cases") {
    applied_min_date <- desired_min_date
    applied_max_date <- desired_max_date
  } else if (parameter == "r") {
    # Shift cases up by mean delay to get projected incidence on day
    applied_min_date <- desired_min_date + mean_delay
    applied_max_date <- desired_max_date + mean_delay
  } else if (parameter == "Rt") {
    # Shift up by mean delay to move to incidence scale and also pad by the
    # GI on either side to prevent missing dates in the convolution
    applied_min_date <- desired_min_date + mean_delay - length(gi_pmf)
    applied_max_date <- desired_max_date + mean_delay + length(gi_pmf)
  }

  seq.Date(
    from = applied_min_date,
    to = applied_max_date,
    by = "day"
  )
}

#' Generate dataframe with values of model covariates to predict
#'
#' We need to parse user-specified input and map it to the necessary
#' input to the model for prediction.
#' @noRd
create_newdata_dataframe <- function(
    object,
    parameter,
    min_date,
    max_date,
    horizon,
    mean_delay,
    gi_pmf,
    delta = delta,
    call = rlang::caller_env()) {
  desired_dates <- parse_predict_dates(
    object = object,
    min_date = min_date,
    max_date = max_date,
    horizon = horizon,
    call = call
  )
  dates <- shift_desired_dates(
    parameter,
    min(desired_dates),
    max(desired_dates),
    mean_delay = mean_delay,
    gi_pmf = gi_pmf
  )
  timesteps <- dates_to_timesteps(
    dates,
    min_supplied_date = object[["min_date"]],
    max_supplied_date = object[["max_date"]]
  )
  if (parameter == "r") {
    # Centered difference timesteps
    # t1: (t0, t2), t2: (t1, t3), ...
    timesteps <- interleave(timesteps - delta, timesteps + delta)
    desired_dates <- interleave(desired_dates, NA)
  } else if (parameter == "Rt") {
    # Use NA padding to mark incomplete (biased) convolution steps to remove
    na_pad <- rep(NA, length(gi_pmf))
    desired_dates <- as.Date(
      c(na_pad, desired_dates, na_pad)
    )
  }

  newdata <- gratia::data_slice(object[["model"]], timestep = timesteps)
  newdata[".row"] <- seq_along(timesteps)
  newdata["reference_date"] <- desired_dates

  newdata
}

#' Format predicted data frame with uniform minimal specification
#' @noRd
format_predicted_dataframe <- function(
    fitted,
    newdata) {
  # Merge with newdata and select required columns
  merged <- merge(fitted,
    newdata,
    by = ".row"
  )[, c(
    "reference_date",
    ".response",
    ".draw"
  )]
  merged$reference_date <- as.Date(merged$reference_date)
  merged[which(!is.na(merged[["reference_date"]])), ]
}

#' Centered difference derivative.
#' @noRd
discrete_diff_derivative <- function(vals) {
  len <- length(vals)
  t0 <- seq(1, len - 1, 2)
  t1 <- seq(2, len, 2)

  (vals[t1] - vals[t0]) / 2
}

#' Generate size in timesteps of a one day step. This one-day
#' step will be applied in either direction of the day of
#' interest to generate a centered two-day window.
#' @noRd
compute_delta <- function(fit) {
  min_date <- fit[["min_date"]]
  timesteps <- dates_to_timesteps(
    c(
      min_date,
      min_date + 1
    ),
    min_supplied_date = min_date,
    max_supplied_date = fit[["max_date"]]
  )
  return(timesteps[2] - timesteps[1])
}

#' Convolve by draw. In the future, additional groups could be detected and
#' specified here.
#' @noRd
rt_by_group <- function(df, group_cols, value_col, vec) {
  df[["group_id"]] <- interaction(df[group_cols], drop = TRUE)
  unique_groups <- as.character(unique(df[["group_id"]]))

  grouped_rt <- lapply(unique_groups, function(group_id) {
    group_df <- df[df[["group_id"]] == group_id, ]
    # Produces vec w/ length nrow(group_df) + length(vec) - 1
    convolved <- stats::convolve(group_df[[value_col]], rev(vec), type = "open")
    # Keep the first N elements of the convolution vector
    conv_ts <- 1:(length(convolved) - length(vec) + 1)
    # Cori method: I_t / \sum_{s = 1}^t{I_{t - s} w_s}
    group_df[".response"] <- group_df[[value_col]] / convolved[conv_ts]
    group_df
  })

  do.call(rbind, grouped_rt)
}
