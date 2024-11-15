# E and I compartments both with exponentially distributed residence times
# with a mean of 4 days.
shape <- 2
rate <- 1 / 4

sir_gt_pmf <- primarycensored::dpcens(0:26,
  pgamma,
  shape = shape,
  rate = rate,
  D = 27
) # v0.4.0

# Drop first element because GI can't have same-day transmission
# and replace with a zero
sir_gt_pmf <- c(0, sir_gt_pmf[2:27])

# Renormalize to a proper PMF
while (abs(sum(sir_gt_pmf) - 1) > 1e-10) {
  sir_gt_pmf <- sir_gt_pmf / sum(sir_gt_pmf)
}

usethis::use_data(sir_gt_pmf, overwrite = TRUE)
