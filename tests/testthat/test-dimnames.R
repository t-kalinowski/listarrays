context("test-dimnames.R")

test_that("setting dimnames works", {
  # ar <- function() array(1:8, 2:4)
  x <- array(1:8, 2:4)
  xyz <- c("x", "y", "z")

  # bare character string sets names of dims
  x_named <- set_dimnames(x, xyz)
  expect_equal(names(dimnames(x_named)), xyz)

  # setting a single dim with a character string works
  y <- set_dimnames(x, xyz, .dim = 2)
  expect_equal(dimnames(y)[[2L]], xyz)

  y <- set_dimnames(x_named, xyz, .dim = 2)
  expect_equal(dimnames(y)[[2L]], xyz)
  expect_equal(names(dimnames(y)), xyz)

  y <- set_dimnames(x_named, xyz, .dim = "y")
  expect_equal(dimnames(y)[[2L]], xyz)
  expect_equal(names(dimnames(y)), xyz)


  # setting with a list works
  y <- set_dimnames(x, list(xyz), .dim = 2)
  expect_equal(dimnames(y)[[2L]], xyz)

  y <- set_dimnames(x_named, list(xyz), .dim = 2)
  expect_equal(dimnames(y)[[2L]], xyz)
  expect_equal(names(dimnames(y)), xyz)

  y <- set_dimnames(x_named, list(xyz), .dim = "y")
  expect_equal(dimnames(y)[[2L]], xyz)
  expect_equal(names(dimnames(y)), xyz)

  y2 <- set_dimnames(x_named, list(y = xyz))
  expect_identical(y, y2)

  y <- set_dimnames(x, list(z = xyz), .dim = 2)
  expect_equal(names(dimnames(y)), c("", "z", ""))
  expect_equal(dimnames(y)[["z"]], xyz)


  # setting more than one dimensionat a time works
  y <- set_dimnames(x, list(a = xyz, b = letters[1:4]), .dim = 2:3)
  expect_equal(names(dimnames(y)), c("", "a", "b"))
  expect_equal(dimnames(y)[["a"]], xyz)
  expect_equal(dimnames(y)[["b"]], letters[1:4])


})
