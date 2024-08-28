test_that("print method produces output for single group", {
  mock_RtGam <- list(
    backend = "MockBackend",
    m = 2,
    k = 5,
    model = list(family = list(family = "poisson", link = "log")),
    data = data.frame(
      reference_date = as.Date("2020-01-01") + 1:10,
      group = NA
    )
  )
  class(mock_RtGam) <- "RtGam"

  expect_snapshot(print(mock_RtGam))
})

test_that("print method produces output for multiple groups", {
  mock_RtGam <- list(
    backend = "MockBackend",
    m = 2,
    k = 5,
    model = list(family = list(family = "poisson", link = "log")),
    data = data.frame(
      reference_date = as.Date("2020-01-01") + 1:10,
      group = c(rep("a", 5), rep("b", 5))
    )
  )
  class(mock_RtGam) <- "RtGam"

  expect_snapshot(print(mock_RtGam))
})

test_that("print method produces output for non-adaptive", {
  mock_RtGam <- list(
    backend = "MockBackend",
    m = 1,
    k = 5,
    model = list(family = list(family = "poisson", link = "log")),
    data = data.frame(
      reference_date = as.Date("2020-01-01") + 1:10,
      group = c(rep("a", 5), rep("b", 5))
    )
  )
  class(mock_RtGam) <- "RtGam"

  expect_snapshot(print(mock_RtGam))
})
