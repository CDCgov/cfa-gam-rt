source("convolve_funs.R")

## test brute-force convolve fun
test <- ((1:10)-5)^2
stopifnot(all.equal(do_convolve(test, delay_pmf),
                    do_convolve2(test, delay_pmf)))
