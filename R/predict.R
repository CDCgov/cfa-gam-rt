#' Predict Method for RtGam Models
#'
#' Generates predictions from an `RtGam` fit. Prediction dates can be specified
#' flexibly using multiple approaches.
#'
#' @param object An `RtGam` object from [RtGam()].
#' @param parameter A string specifying the prediction parameter. Options are
#'   `"obs_cases"`, `"incidence"`, `"r"`, and `"Rt"`. Defaults to `"obs_cases"`.
#' @param horizon Optional. Integer specifying forecast days from the last date
#'   in the fit. For example, `horizon = 7` returns a 7-day forecast.
#' @param min_date,max_date Optional. A date-like object. See details for more
#'   information on specification.
#' @param n Number of posterior samples to use. Default is 100.
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
#' 4     2023-01-01        11     2
#' 5     2023-01-02        19     2
#' 6     2023-01-03        24     2
##' ```
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
  } else if (parameter == "obs_incidence") {
    predict_obs_incidence(
      object = object,
      horizon = horizon,
      min_date = min_date,
      max_date = max_date,
      n = n,
      mean_delay = mean_delay,
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
    horizon = horizon
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
  format_predicted_dataframe(parameter = "obs_cases", fitted, newdata)
}

#' @rdname predictor
#' @export
#' @export
predict_obs_incidence <- function(
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
    parameter = "obs_incidence",
    mean_delay = mean_delay,
    min_date = min_date,
    max_date = max_date,
    horizon = horizon
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
  format_predicted_dataframe(
    parameter = "obs_incidence",
    fitted, newdata
  )
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
    delta = delta
  )
  fitted <- gratia::fitted_samples(
    object[["model"]],
    data = newdata,
    n = n,
    seed = seed,
    unconditional = TRUE,
    scale = "linear_predictor",
    ...
  )
  format_predicted_dataframe(
    parameter = "r",
    fitted = fitted,
    newdata = newdata,
    delta = delta
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
    gi_pmf = gi_pmf
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
  format_predicted_dataframe(
    parameter = "Rt",
    fitted = fitted,
    newdata = newdata,
    gi_pmf = gi_pmf
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

#' Convert from user-specified dates to internal timesteps
#'
#' @return Double vector, the timesteps to predict
#' @noRd
prep_timesteps_for_pred <- function(
    parameter,
    fit_min_date,
    fit_max_date,
    desired_min_date,
    desired_max_date,
    mean_delay,
    call = rlang::caller_env()) {
  dates <- shift_desired_dates(
    parameter,
    desired_min_date,
    desired_max_date
  )
  dates_to_timesteps(
    dates,
    min_supplied_date = fit_min_date,
    max_supplied_date = fit_max_date
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
  } else if (parameter == "obs_incidence" || parameter == "r") {
    # Shift cases up by mean delay to get projected incidence on day
    applied_min_date <- desired_min_date + mean_delay
    applied_max_date <- desired_max_date + mean_delay
  } else if (parameter == "Rt") {
    # Shift up by mean delay to move to incidence scale and also pad by the
    # GI on either side to prevent missing dates in the convolution
    applied_min_date <- desired_min_date + mean_delay - length(gi_pmf)
    applied_max_date <- desired_max_date + mean_delay + length(gi_pmf)
  } else {
    cli::cli_abort("Parameter spelled wrong. Impossible to reach")
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
    timesteps <- interleave(timesteps - delta, timesteps + delta)
    desired_dates <- interleave(desired_dates, NA)
  } else if (parameter == "Rt") {
    # Use NA padding to mark incomplete sections to remove
    na_pad <- rep(NA, length(gi_pmf))
    desired_dates <- as.Date(
      c(na_pad, desired_dates, na_pad)
    )
  }
  format_newdata_dataframe(
    fit = object,
    parameter = parameter,
    desired_dates = desired_dates,
    timesteps = timesteps
  )
}

#' Format predicted data frame with uniform minimal specification
#' @noRd
format_predicted_dataframe <- function(
    parameter,
    fitted,
    newdata,
    delta,
    gi_pmf) {
  if (parameter == "r") {
    # Difference calculation for `r` parameter
    timestep_first_row <- which((fitted[[".row"]] - 1) %% 2 == 0)
    fitted <- data.frame(
      .row = (fitted[timestep_first_row, ".row"] + 1) / 2,
      .response = discrete_diff_derivative(fitted[[".fitted"]]),
      .draw = fitted[timestep_first_row, ".draw"]
    )
  } else if (parameter == "Rt") {
    # Rt calculation
    fitted <- rt_by_group(fitted,
      group_cols = ".draw",
      value_col = ".fitted",
      vec = gi_pmf
    )
  }

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

  return(merged)
}

#' Turn input parameters into a dataframe to pass to model
#' @noRd
format_newdata_dataframe <- function(
    fit,
    parameter,
    desired_dates,
    timesteps,
    gi_pmf,
    delta) {
  newdata <- gratia::data_slice(fit[["model"]], timestep = timesteps)

  if (parameter == "r") {
    # Join key when adding metadata to posterior sim. Setting
    # every other row to NA ensures we get one row per date-draw.
    newdata[".row"] <- interleave(seq(1, length(timesteps) / 2, 1), NA)
  } else {
    newdata[".row"] <- seq_along(timesteps)
  }
  newdata["reference_date"] <- desired_dates
  return(newdata)
}

#' Centered difference derivative.
#' @noRd
discrete_diff_derivative <- function(vals) {
  len <- length(vals)
  t0 <- seq(1, len - 1, 2)
  t1 <- seq(2, len, 2)

  (vals[t1] - vals[t0]) / 2
}

#' Generate size in timesteps of centered two day window
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
