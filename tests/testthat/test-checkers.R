test_that("Type error throws successfully", {
  object <- "Test string"
  arg_name <- "Test argument"
  expected_type <- "integer"
  # Turns off call env -- we just want to check it throws
  call <- NULL

  expect_error(
    obj = throw_type_error(object, arg_name, expected_type, call),
    class = "RtGam_type_error"
  )
})
