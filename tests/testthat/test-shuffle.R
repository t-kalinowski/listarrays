context("test-shuffle.R")

test_that("shuffle_rows", {
  x <- 1:3
  y <- matrix(1:9, ncol = 3)
  z <- array(1:27, c(3,3,3))

  set.seed(1)
  l <- shuffle_rows(x, y, z)

  i <- c(1L, 3L, 2L)
  expect_identical(x[i],   l[[1]])
  expect_identical(y[i,],  l[[2]])
  expect_identical(z[i,,], l[[3]])

  set.seed(1)
  l <- shuffle_rows(x, y, z, in_sync = FALSE)
  expect_identical(x[c(1,3,2)],   l[[1]])
  expect_identical(y[c(3,1,2),],  l[[2]])
  expect_identical(z[c(3,2,1),,], l[[3]])


})
