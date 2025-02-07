test_that("Penalty heuristic fails for bad inputs", {
  # No input
  expect_error(penalty_dim_heuristic(),
    class = "rlang_error"
  )
  # Not a vector
  expect_error(penalty_dim_heuristic(list(1:10)),
    class = "RtGam_type_error"
  )
  # Not an integer-ish
  expect_error(penalty_dim_heuristic(c("a", "b")),
    class = "RtGam_type_error"
  )
  # Not non-neg
  expect_error(penalty_dim_heuristic(-2),
    class = "RtGam_invalid_input"
  )
  # Not a single number
  expect_error(penalty_dim_heuristic(c(1, 2)),
    class = "RtGam_invalid_input"
  )
})

test_that("Penalty heuristic works for good inputs", {
  # n for n \le 23
  expect_equal(penalty_dim_heuristic(1), 1L)
  expect_equal(penalty_dim_heuristic(20), 1L)

  expect_equal(penalty_dim_heuristic(56), 2L)
  expect_equal(penalty_dim_heuristic(120), 3L)
})
