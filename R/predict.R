#' Predict Method for RtGam Models
#'
#' Generates predictions from an `RtGam` fit. Prediction dates can be specified
#' flexibly using multiple approaches.
#'
#' @param object An `RtGamFit` object from [RtGam()].
#' @param type A string specifying the prediction type. Options are
#'   `"obs_cases"`, `"incidence"`, `"r"`, and `"Rt"`. Currently, only
#'   `"obs_cases"` is supported. Matching is enforced by [rlang::arg_match()].
#' @param horizon Optional. Integer specifying forecast days from the last date
#'   in the fit. For example, `horizon = 7` returns a 7-day forecast.
#' @param min_date Optional. A date-like object marking the start of the
#'   prediction period.
#' @param max_date Optional. A date-like object marking the end of the
#'   prediction period.
#' @param n Number of posterior samples to use. Default is 10.
#' @param mean_delay Optional. Numeric mean delay used in the prediction.
#' @param gi_pmf Optional. A vector representing the generation interval PMF.
#' @param seed Random seed for reproducibility. Default is 12345.
#' @param ... Additional arguments passed to lower-level functions.
#'
#' @details
#' Prediction dates can be set in four ways:
#'
#' 1. **Using Fit Object Alone**: Predictions span the full date range in the
#'    original model fit.
#' 2. **Using `horizon`**: Forecasts extend `horizon` days from the fit’s last
#'    date.
#' 3. **Using `min_date` and `horizon`**: Predictions start at `min_date` and
#'    end `horizon` days after the fit’s last date.
#' 4. **Using `min_date` and `max_date`**: Predictions span all dates between
#'    these two (inclusive).
#'
#' @return
#' A dataframe in [tidy format](https://www.jstatsoft.org/article/view/v059i10),
#' with each row representing a draw for a specific date:
#'
#' - `reference_date`: Date of the prediction.
#' - `.response`: Predicted value (e.g., observed cases).
#' - `.draw`: ID of the posterior draw.
#'
#' Example output:
#' ```
#'   reference_date .response .draw
#' 1     2023-01-01        18     1
#' 2     2023-01-02        13     1
#' 3     2023-01-03        21     1
#' ```
#'
#' @export
predict.RtGam <- function(
    object,
    type,
    horizon = NULL,
    min_date = NULL,
    max_date = NULL,
    n = 10,
    mean_delay = NULL,
    gi_pmf = NULL,
    seed = 12345,
    ...) {
  rlang::arg_match(type,
    values = c(
      "obs_cases",
      "incidence",
      "r",
      "Rt"
    ),
    call = rlang::caller_env()
  )
  if (type != "obs_cases") {
    if (rlang::is_null(mean_delay)) {
      cli::cli_abort("{.arg mean_delay} is required when type is {.val {type}}")
    }
    check_integer(mean_delay, "gi_pmf")
  }
  if (type == "Rt") {
    if (rlang::is_null(gi_pmf)) {
      cli::cli_abort("{.arg gi_pmf} is required when type is {.val Rt}")
    }
    check_vector(gi_pmf, "gi_pmf")
    check_no_missingness(gi_pmf, "gi_pmf")
    check_elements_above_min(gi_pmf, "gi_pmf", 0)
    check_elements_below_max(gi_pmf, "gi_pmf", 1)
    check_sums_to_one(gi_pmf, "gi_pmf")
  }

  desired_dates <- parse_predict_dates(
    object = object,
    min_date = min_date,
    max_date = max_date,
    horizon = horizon
  )
  timesteps <- prep_timesteps_for_pred(
    type = type,
    desired_min_date = min(desired_dates),
    desired_max_date = max(desired_dates),
    fit_min_date = object[["min_date"]],
    fit_max_date = object[["max_date"]],
    mean_delay = mean_delay
  )

  if (type == "obs_cases") {
    predict_obs_cases(
      object,
      desired_dates,
      timesteps,
      n = n,
      seed = seed,
      ...
    )
  } else {
    cli::cli_abort("{.val {type}} not yet implemented}")
  }
}

#' Posterior predicted cases
#' @noRd
predict_obs_cases <- function(object, desired_dates, timesteps, n, seed, ...) {
  newdata <- data.frame(
    timestep = timesteps,
    .row = seq_along(timesteps),
    reference_date = desired_dates
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

  merged <- merge(fitted,
    newdata,
    by = ".row"
  )
  data.frame(
    reference_date = merged[["reference_date"]],
    .response = merged[[".response"]],
    .draw = merged[[".draw"]]
  )
}

#' Convert from user specification to necessary date range
#'
#' @inheritParams predict.RtGam
#' @param call The calling environment to be reflected in the error message
#'
#' @return List with two elements: min_date and max_date
#' @keyword internal
#' @importFrom rlang %||%
parse_predict_dates <- function(
    object,
    min_date = NULL,
    max_date = NULL,
    horizon = NULL,
    call = rlang::caller_env()) {
  if (!rlang::is_null(min_date)) check_date(min_date, call = call)
  if (!rlang::is_null(max_date)) check_date(max_date, call = call)
  if (!rlang::is_null(horizon)) check_integer(horizon, call = call)

  # Handle horizon to estimate dates if provided
  if (!rlang::is_null(horizon)) {
    if (!rlang::is_null(max_date)) {
      cli::cli_abort("Cannot specify both {.arg horizon} and {.arg max_date}",
        call = call
      )
    }
    min_date <- min_date %||% (object[["max_date"]] + 1)
    max_date <- object[["max_date"]] + horizon + 1
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

#' Convert from user-specified dates to internal timesteps
#'
#' @inheritParams predict.RtGam
#' @return Double vector, the timesteps to predict
prep_timesteps_for_pred <- function(
    type,
    fit_min_date,
    fit_max_date,
    desired_min_date,
    desired_max_date,
    mean_delay,
    call = rlang::caller_env()) {
  dates <- shift_desired_dates(
    type,
    desired_min_date,
    desired_max_date
  )
  dates_to_timesteps(
    dates,
    min_supplied_date = fit_min_date,
    max_supplied_date = fit_max_date
  )
}

#' Map user-req'd dates to model-req'd dates. Incidence is a
#' mean-shift ahead of cases and Rt requires GI padding for the
#' convolution
#' @noRd
shift_desired_dates <- function(
    type,
    desired_min_date,
    desired_max_date,
    mean_delay,
    gi_pmf) {
  if (type == "incidence" || type == "growth_rate") {
    # Shift cases up by mean delay to get projected incidence on day
    desired_min_date <- desired_min_date + mean_delay
    desired_max_date <- desired_max_date + mean_delay
  } else if (type == "Rt") {
    # Shift up by mean delay to move to incidence scale and also pad by the
    # GI on either side to prevent missing dates in the convolution
    desired_min_date <- desired_min_date + desired_mean_delay - length(gi_pmf)
    desired_max_date <- desired_max_date + mean_delay + length(gi_pmf)
  }

  seq.Date(
    from = desired_min_date,
    to = desired_max_date,
    by = "day"
  )
}
