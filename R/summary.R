summary.RtGam <- function(
    object,
    type,
    horizon = 14,
    min_date = NULL,
    max_date = NULL,
    ...) {
  rlang::arg_match(type,
    values = c(
      "obs_cases",
      "latent_cases",
      "r",
      "Rt"
    )
  )
  if (rlang::is_null(min_date)) {
    min_date <- object[["min_date"]]
  }
  if (rlang::is_null(max_date)) {
    max_date <- object[["max_data"]]
  }

  if (min_date >= max_date) {
    rlang::abort(c(
      "{.arg min_date} must be greater than {.arg max_date}",
      "{.arg {min_date}}: {.val {min_date}}",
      "{.arg max_date}: {.val {max_date}}"
    ))
  }
  desired_dates <- seq.Date(
    from = min_date,
    to = max_date + horizon,
    by = "day"
  )
  timesteps <- dates_to_timesteps(desired_dates,
    min_supplied_date = object[["min_date"]],
    max_supplied_date = object[["max_data"]]
  )

  newdata <- data.frame(
    timestep = timesteps,
    .row = seq_along(timesteps),
    reference_date = desired_dates
  )

  fitted <- gratia::posterior_samples(object[["model"]],
    data = newdata, ...
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
