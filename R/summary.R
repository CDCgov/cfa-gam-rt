predict.RtGam <- function(
    object,
    type,
    horizon = NULL,
    min_date = NULL,
    max_date = NULL,
    n = 10,
    mean_delay = NULL,
    seed = 12345,
    ...) {
  rlang::arg_match(type,
    values = c(
      "obs_cases",
      "incidence",
      "r",
      "Rt"
    )
  )
  # If horizon, estimate from the last date to the forecast horizon
  if (!rlang::is_null(horizon)) {
    rlang::check_exclusive(max_date, horizon)
    if (rlang::is_null(min_date)) {
      min_date <- object[["max_date"]] + 1
    }
    max_date <- object[["max_date"]] + horizon
  }
  # Else if nothing is specified, estimate period w/ obs data
  if (rlang::is_null(min_date)) {
    min_date <- object[["min_date"]]
  }
  if (rlang::is_null(max_date)) {
    max_date <- object[["max_date"]]
  }

  if (min_date >= max_date) {
    rlang::abort(c(
      "{.arg min_date} must be greater than {.arg max_date}",
      "{.arg {min_date}}: {.val {min_date}}",
      "{.arg max_date}: {.val {max_date}}"
    ))
  }

  if (type == "obs_cases") {
    desired_dates <- seq.Date(
      from = min_date,
      to = max_date,
      by = "day"
      # TODO: does this handle doubles properly? How should they be handled?
    )

    timesteps <- dates_to_timesteps(desired_dates,
      min_supplied_date = object[["min_date"]],
      max_supplied_date = object[["max_date"]]
    )
    newdata <- data.frame(
      timestep = timesteps,
      .row = seq_along(timesteps),
      reference_date = desired_dates
    )

    fitted <- gratia::posterior_samples(
      object[["model"]],
      data = newdata,
      unconditional = TRUE,
      ...
    )
    print(head(fitted))
  } else if (type == "incidence") {
    # I'm breaking it out because we're going to want to also remove day of week effect I think.
    if (!rlang::is_bare_numeric(mean_delay)) {
      cli::cli_abort("{.arg mean_delay} is required when {.arg type} is {.val obs_incidence}")
    }

    # Extract incidence at mean-shifted dates
    desired_dates <- seq.Date(
      from = min_date,
      to = max_date,
      by = "day"
      # TODO: does this handle doubles properly? How should they be handled?
    )

    # Shift desired dates _forward_ to get the corresponding date in fitted cases
    mean_shifted_dates <- desired_dates + mean_delay


    timesteps <- dates_to_timesteps(mean_shifted_dates,
      min_supplied_date = object[["min_date"]],
      max_supplied_date = object[["max_date"]]
    )

    # And associate the timestep with the incidence date, not the case date to shift back from cases to incidence
    newdata <- data.frame(
      timestep = timesteps,
      .row = seq_along(timesteps),
      reference_date = desired_dates
    )

    # Use `posterior_samples()` over `fitted_samples()` to get response w/ obs uncertainty
    fitted <- gratia::posterior_samples(
      object[["model"]],
      data = newdata,
      unconditional = TRUE,
      ...
    )
  } else if (type == "r") {
    if (!rlang::is_bare_numeric(mean_delay)) {
      cli::cli_abort("{.arg mean_delay} is required when {.arg type} is {.val obs_incidence}")
    }

    # Extract incidence at mean-shifted dates
    desired_dates <- seq.Date(
      from = min_date,
      to = max_date,
      iy = "day"
    )

    # Shift desired dates _forward_ to get the corresponding date in fitted cases
    mean_shifted_dates <- desired_dates + mean_delay


    timesteps <- dates_to_timesteps(mean_shifted_dates,
      min_supplied_date = object[["min_date"]],
      max_supplied_date = object[["max_date"]]
    )

    # Construct timesteps as with incidence and shift by delta for discrete derivative
    newdata <- data.frame(
      timestep = timesteps,
      reference_date = desired_dates
    )
    delta <- 1e-9
    timesteps_shifted <- timesteps + delta
    print(head(timesteps))
    print(head(timesteps_shifted))
    all_timesteps <- c(rbind(timesteps, timesteps_shifted))
    print(head(all_timesteps))
    # `rbind()` interleaves vectors, so we can difference them
    ds <- gratia::data_slice(fit[["model"]], timestep = all_timesteps)

    all_timesteps <- gratia::fitted_samples(
      object[["model"]],
      data = ds,
      n = n,
      seed = seed,
      unconditional = TRUE,
      scale = "response",
      ...
    )
    is_orig_timestep <- all_timesteps[[".row"]] %% 2 == 1
    differenced_draws <- all_timesteps[[".fitted"]][is_orig_timestep] - all_timesteps[[".fitted"]][!is_orig_timestep]
    print(head(differenced_draws))
    growth_rate <- differenced_draws[is_orig_timestep] / delta
    print(head(growth_rate))
    fitted <- data.frame(
      .response = growth_rate,
      .row = 1:length(growth_rate),
      .draw = all_timesteps[[".draw"]][is_orig_timestep],
      timestep = timesteps
    )
    print(head(fitted))
  } else {
    cli::cli_abort("Not implemented")
  }

  merged <- merge(fitted,
    newdata,
    by = "timestep"
  )

  data.frame(
    reference_date = merged[["reference_date"]],
    .response = merged[[".response"]],
    .draw = merged[[".draw"]]
  )
}
