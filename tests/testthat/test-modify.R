context("test-modify.R")

test_that("modify_along_dim works", {
  x <- array(1:840, 4:7)

  for (d in seq_along(4:7)) {
    expect_identical(x, modify_along_dim(x, d, identity))
  }

  # accepts returned without dim

  for (d in seq_along(4:7))
    expect_identical(x, modify_along_dim(x, d, as.vector))

  cx <- array(as.character(x), dim = 4:7)

  for (d in seq_along(4:7))
    expect_identical(cx, modify_along_dim(x, d, paste))


  # works with more than one dimension
  for (d in combn(1:length(dim(x)), 2, simplify = FALSE)) {
    expect_identical(x, modify_along_dim(x, d, identity))
  }

  for (d in combn(1:length(dim(x)), 3, simplify = FALSE)) {
    expect_identical(x, modify_along_dim(x, d, identity))
  }

  # go big enough to trigger calling cmpfun()
  arr <- function(...) array(seq_len(prod(c(...))), c(...))
  x <- arr(5:9)
  expect_identical(x, modify_along_dim(x, 3:5, identity))

})
