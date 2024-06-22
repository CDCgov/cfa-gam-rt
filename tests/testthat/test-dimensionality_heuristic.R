test_that("Dimensionality heuristic fails for bad inputs", {
  # No input
  expect_error(smooth_dim_heuristic(),
    class = "rlang_error"
  )
  # Not a vector
  expect_error(smooth_dim_heuristic(list(1:10)),
    class = "RtGam_type_error"
  )
  # Not an integer-ish
  expect_error(smooth_dim_heuristic(c("a", "b")),
    class = "RtGam_type_error"
  )
  # Not non-neg
  expect_error(smooth_dim_heuristic(-2),
    class = "RtGam_invalid_input"
  )
  # Not a single number
  expect_error(smooth_dim_heuristic(c(1, 2)),
    class = "RtGam_invalid_input"
  )
})

test_that("Dimensionality heuristic works for good inputs", {
  # n for n \le 10
  expect_equal(1L, smooth_dim_heuristic(1))
  expect_equal(10L, smooth_dim_heuristic(10))
  # ceil(sqrt(10 * n)) for n > 10
  # sqrt(10 * 11) \approx 10.48 -- checks that we're using ceil()
  expect_equal(11L, smooth_dim_heuristic(11))
  expect_equal(32L, smooth_dim_heuristic(100))
})
