#' Plot method for RtGam fits
#'
#' Generates predicted draws from [predict.RtGam] and plots them with ggplot2.
#' See the documentation on [predict.RtGam] for options on how to specify
#' draws.
#'
#' @inheritParams predict.RtGam
#' @param x An `RtGam` object created using the [RtGam()]
#' function.
#' @param alpha The opacity of the lines on the plot. Should be between 0 and 1,
#' with values closer to 0 producing more transparent lines.
#' @param ... Additional options to pass to [predict.RtGam()]
#' @returns A ggplot2 object
#' @examples
#' fit <- RtGam(
#'   stochastic_sir_rt[["obs_cases"]],
#'   stochastic_sir_rt[["reference_date"]]
#' )
#' # Plot draws from the fitted model against the data
#' plot(fit)
#'
#' # Plot a 7-day forecast
#' plot(fit, horizon = 7)
#'
#' # Plot Rt estimates
#' plot(fit,
#'   parameter = "Rt",
#'   mean_delay = 0,
#'   gi_pmf = sir_gt_pmf
#' )
#' @export
plot.RtGam <- function(x, parameter = "obs_cases", alpha = 0.05, ...) {
  preds <- predict.RtGam(x, parameter = parameter, ...)

  # Hack to get around lint warnings for NSE
  reference_date <- cases <- .response <- .draw <- NULL

  if (parameter == "obs_cases") {
    p <- ggplot2::ggplot() +
      ggplot2::geom_point(
        ggplot2::aes(reference_date, cases),
        data = x[["data"]]
      ) +
      ggplot2::geom_line(
        ggplot2::aes(reference_date, .response, group = .draw),
        alpha = alpha,
        data = preds
      ) +
      ggplot2::theme_bw() +
      ggplot2::labs(x = "Reference date", y = "Cases")
  }
  if (parameter == "r") {
    p <- ggplot2::ggplot() +
      ggplot2::geom_hline(
        ggplot2::aes(yintercept = 0),
        alpha = 0.5,
        data = preds
      ) +
      ggplot2::geom_line(
        ggplot2::aes(reference_date, .response, group = .draw),
        alpha = alpha,
        data = preds
      ) +
      ggplot2::theme_bw() +
      ggplot2::labs(x = "Reference date", y = "Intrinsic growth rate (r)")
  }
  if (parameter == "Rt") {
    p <- ggplot2::ggplot() +
      ggplot2::geom_hline(
        ggplot2::aes(yintercept = 1),
        alpha = 0.5,
        data = preds
      ) +
      ggplot2::geom_line(
        ggplot2::aes(reference_date, .response, group = .draw),
        alpha = alpha,
        data = preds
      ) +
      ggplot2::theme_bw() +
      ggplot2::labs(x = "Reference date", y = "Rt") +
      ggplot2::scale_y_continuous(
        trans = "log",
        labels = scales::number_format(accuracy = 0.01)
      )
  }
  return(p)
}
