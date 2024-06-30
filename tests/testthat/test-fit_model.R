test_that("fit_model() fits a model", {
  data <- data.frame(x = 1:20, y = rnbinom(20, mu = 1:20, size = 1))
  formula <- y ~ 1 + s(x)

  fit_gam <- fit_model(data, formula, backend = "gam")
  fit_bam <- fit_model(data, formula, backend = "bam")

  expect_s3_class(fit_gam, "gam")
  expect_s3_class(fit_bam, "bam")
})

test_that("arg_constructor returns fitting args", {
  data <- data.frame(x = 1, y = 2)
  formula <- y ~ x
  backend <- "gam"

  args <- args_constructor(data, formula, backend)

  expect_type(args$formula, "language")
  expect_s3_class(args$data, "data.frame")
  expect_equal(args$family, "nb")

  # `mgcv::gam()` backend
  expect_equal(args$method, "REML")

  # `mgcv::bam()` backend
  args_bam <- args_constructor(data, formula, backend = "bam")
  expect_equal(args_bam$method, "fREML")

  # Other backend
  expect_error(args_constructor(data, formula, "not_a_backend"))
})

test_that("call_constructor returns a call", {
  backend <- "gam"

  call <- call_constructor(backend)
  expect_type(call, "closure")
  expect_equal(call, mgcv::gam)
})
