test_that("Smooth heuristic fails for bad inputs", {
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

test_that("Smooth heuristic works for good inputs", {
  # n for n \le 12
  expect_equal(1L, smooth_dim_heuristic(1))
  expect_equal(12L, smooth_dim_heuristic(12))
  # ceil(sqrt(12 * n)) for n > 12
  # sqrt(12 * 11) \approx 11.48 -- checks that we're using ceil()
  expect_equal(11L, smooth_dim_heuristic(11))
  expect_equal(35L, smooth_dim_heuristic(100))
})
