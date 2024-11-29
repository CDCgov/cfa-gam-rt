# Start with the original Gostic, 2020 dataset
gostic2020 <- readRDS(file.path("data-raw", "gostic2020.rds"))

# Add a date col for package use, abitrarily starting from Jan 1, 2023
gostic2020["reference_date"] <- gostic2020[["time"]] + as.Date("2023-01-01") - 1

# Add an outcome col with a day of week effect

# Use a sine curve to make effect large and obvious
dow_effect <- 0.5 * cos(seq(from = 0, to = pi, length.out = 7))
nrep <- ceiling(nrow(gostic2020) / 7)
# Get dims to match
dow_col <- rep(dow_effect, nrep)[seq_len(nrow(gostic2020))]
gostic2020["dow"] <- dow_col

# Apply dow to log expected cases
gostic2020["incidence_dow"] <- exp(log(gostic2020[["incidence"]]) + dow_col)

# Apply outcome noise
withr::with_seed(12345, {
  gostic2020["obs_cases"] <- as.integer(rnbinom(
    nrow(gostic2020),
    mu = gostic2020[["incidence_dow"]],
    size = 10
  ))
  gostic2020["obs_cases_no_dow"] <- as.integer(rnbinom(
    nrow(gostic2020),
    mu = gostic2020[["incidence"]],
    size = 10
  ))
})

# Subset to cols of interest
cols <- c(
  "reference_date",
  "true_rt",
  "dow",
  "incidence",
  "obs_cases",
  "obs_cases_no_dow"
)

stochastic_sir_rt <- gostic2020[, cols]
usethis::use_data(stochastic_sir_rt, overwrite = TRUE)
