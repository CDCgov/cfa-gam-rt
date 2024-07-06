test_that("fit_model.RtGam_gam fits a model", {
  data <- data.frame(x = 1:20, y = rnbinom(20, mu = 1:20, size = 1))
  class(data) <- c("RtGam_gam", class(data))
  formula <- y ~ 1 + s(x)

  fit_gam <- fit_model(data, formula, list())

  expect_s3_class(fit_gam, "gam")
})

test_that("fit_model.RtGam_bam fits a model", {
  data <- data.frame(x = 1:20, y = rnbinom(20, mu = 1:20, size = 1))
  class(data) <- c("RtGam_bam", class(data))
  formula <- y ~ 1 + s(x)

  fit_gam <- fit_model(data, formula, list())

  expect_s3_class(fit_gam, "bam")
})
