# E and I compartments both with exponentially distributed residence times
# with a mean of 4 days.
shape <- 2
rate <- 1 / 4

gostic_gt_pmf <- primarycensoreddist::dpcens(0:26,
  pgamma,
  shape = shape,
  rate = rate,
  D = 27
) # v0.4.0

# Drop first element because GI can't have same-day transmission
gostic_gt_pmf <- gostic_gt_pmf[2:27]

# Renormalize to a proper PMF
while (abs(sum(gostic_gt_pmf) - 1) > 1e-10) {
  gostic_gt_pmf <- gostic_gt_pmf / sum(gostic_gt_pmf)
}

usethis::use_data(gostic_gt_pmf, overwrite = TRUE)
