context("test-split.R")

test_that("split_along_dim", {
  x <- array(1:840, 4:7)

  for (d in seq_along(dim(x)))
    expect_length(split_along_dim(x, d), dim(x)[d])

})


test_that("split_on_dim", {
  x <- array(1:840, 4:7)

  for (d in seq_along(dim(x))) {
    f <- rep_len(1:2, dim(x)[d])
    expect_length(split_on_dim(x, d, f), 2L)
  }

})


test_that("split* works recursively", {
  x1 <- 1:4
  x2 <- matrix(1:20, nrow = 4, ncol = 5)
  x3 <- array(1:840, 4:7)

  l <- split_along_rows(list(x1, x2, x3))

  for (i in seq_along(l)) {
    expect_length(l[[i]], 4)
  }

})

test_that("split_on f inputs", {
  x <- array(1:840, 4:7)

  # test a scalar integer
  l <- split_on_rows(x, 4)
  expect_length(l, 4L)
  for (i in seq_along(l))
    expect_equal(dim(l[[i]]), c(1, 5:7))

  l <- split_on_rows(x, 4, drop = TRUE)
  expect_length(l, 4L)
  for (i in seq_along(l))
    expect_equal(dim(l[[i]]), 5:7)

  # test a vector of proportions
  x <- bind_as_rows(rep_len(list(array(1:8, 2:4)), 10))
  l <- split_on_rows(x, c(0.2, 0.2, 0.6), drop = FALSE)
  expect_length(l, 3L)
  expect_equal(nrow(l[[1]]), 2L)
  expect_equal(nrow(l[[2]]), 2L)
  expect_equal(nrow(l[[3]]), 6L)

})



test_that("c impl of split_along_rows works", {
  m <-  matrix(1:20, nrow = 4, ncol = 5)


  for (mode in c("integer", "double", "complex", "logical")) {
    storage.mode(m) <- mode
    identical(.split_along_rows(m),
              lapply(1:4, function(r) m[r, ]))
  }

})
