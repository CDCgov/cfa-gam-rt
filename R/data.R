# nolint start: line_length_linter
#' Synthetic dataset of stochastic SIR system with known Rt
#'
#' A simulated dataset derived from Gostic, Katelyn M., et al. "Practical
#' considerations for measuring the effective reproductive number, Rt."
#' PLoS Computational Biology 16.12 (2020): e1008409. The data are simulated
#' from a stochastic SEIR compartmental model. The original timeseries and Rt
#' are available in the `incidence` and `true_rt` columns and with additional
#' columns added or modified to increase noise and add a day-of-week effect.
#'
#' This synthetic dataset has a number of desirable properties:
#'
#' 1. The force of infection changes depending on the Rt, allowing for sudden
#' changes in the Rt. This allows for modeling of sudden changes in infection
#' dynamics, which might otherwise be difficult to capture.
#'
#' 2. The realized Rt is known at each timepoint
#'
#' 3. The dataset incorporates a simple generation interval and a reporting
#' delay.
#'
#' Gostic et al. benchmark the performance of a number of Rt estimation
#' frameworks, providing practical guidance on how to use this dataset to
#' evaluate Rt estimates.
#'
#' In practice, we've found that the amount of observation noise is often
#' undesirably low for testing. Many empirical datasets are much noisier. As a
#' result, models built with these realistically noisy settings in mind can
#' perform poorly on this dataset or fail to converge. To better reflect realistic
#' settings, we manually add observation noise and a day-of-week reporting effect.
#'
#' @name stochastic_sir_rt
#' @format `stochastic_sir_rt` A data frame with 299 rows and 6 columns:
#' \describe{
#'    \item{reference_date}{The date cases were observed.}
#'    \item{true_rt}{The known, true Rt of the epidemic system.}
#'    \item{dow}{The magnitude of the day-of-week effect added to the log of `incidence`.}
#'    \item{true_cases}{The true number of cases occurring on the `date` in the
#'       simulated system before observation noise or day-of-week effects.}
#'    \item{obs_cases}{The observed number of cases on `date` after the day-of-week
#'      reporting effect and observation noise have been applied to `true_cases`.}
#'    \item{obs_cases_no_dow}{The observed number of cases on `date` after observation
#'      noise has been applied to `true_cases`. It does not include a day of week effect.}
#' }
#' @source
#' <https://github.com/cobeylab/Rt_estimation/tree/d9d8977ba8492ac1a3b8287d2f470b313bfb9f1d>
#' <https://github.com/CDCgov/cfa-epinow2-pipeline/pull/91>
#' <https://github.com/CDCgov/cfa-epinow2-pipeline/pull/17>
#' @references Gostic, Katelyn M., et al. "Practical considerations for measuring the
#'   effective reproductive number, Rt." PLoS computational biology 16.12
#'   (2020): e1008409.
"stochastic_sir_rt"

#' Generation interval corresponding to the sample `stochastic_sir_rt` dataset
#'
#' Gostic et al., 2020 simulates data from a stochastic SEIR model. Residence
#' time in both the E and the I compartments is exponentially distributed, with
#' a mean of 4 days (or a rate/inverse-scale of 1/4). These residence times
#' imply a gamma-distributed generation time distribution with a shape of 2 and
#' a rate of 1/4. The distribution can be regenerated in
#' `data-raw/sir_gt_pmf.R`.
#'
#' From this parametric specification, we produce a double-censored,
#' left-truncated probability mass function of the generation interval
#' distribution. We produce the PMF using
#' [primarycensored::dpcens()] with version 0.4.0. See
#' https://doi.org/10.1101/2024.01.12.24301247 for more information on
#' double-censoring biases and corrections.
#'
#' @name sir_gt_pmf
#' @format `sir_gt_pmf` A numeric vector of length 26 that sums to one within
#'   numerical tolerance
#' @references Gostic, Katelyn M., et al. "Practical considerations for measuring the
#'   effective reproductive number, Rt." PLoS computational biology 16.12
#'   (2020): e1008409.
# nolint end: line_length_linter
"sir_gt_pmf"
