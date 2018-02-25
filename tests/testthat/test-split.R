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
