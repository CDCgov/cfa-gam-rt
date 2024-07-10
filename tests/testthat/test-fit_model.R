test_that("fit_model.RtGam_gam fits a model", {
  data <- data.frame(x = 1:20, y = rnbinom(20, mu = 1:20, size = 1))
  class(data) <- c("RtGam_gam", class(data))
  formula <- y ~ 1 + s(x)

  fit_gam <- fit_model(data, formula)

  expect_s3_class(fit_gam, "gam")
})

test_that("fit_model.RtGam_bam fits a model", {
  data <- data.frame(x = 1:20, y = rnbinom(20, mu = 1:20, size = 1))
  class(data) <- c("RtGam_bam", class(data))
  formula <- y ~ 1 + s(x)

  fit_gam <- fit_model(data, formula)

  expect_s3_class(fit_gam, "bam")
})

test_that("... passes modified arg to `fit_model()`", {
  data <- data.frame(x = 1:20, y = rnbinom(20, mu = 1:20, size = 1))
  class(data) <- c("RtGam_gam", class(data))
  formula <- y ~ 1 + s(x)

  fit_gam <- fit_model(data, formula, family = "poisson")

  expect_s3_class(fit_gam, "gam")
  expect_true(grepl("poisson", fit_gam$family$family))
})
