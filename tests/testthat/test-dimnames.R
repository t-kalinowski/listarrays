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



test_that("specifying `.dim` by name works", {

  x <- provideDimnames(array(1:8, 2:4))
  x <- set_dimnames(x, paste0("axis", 1:3))

  expect_identical(extract_dim(x, "axis2", 1),
                   extract_dim(x, 2L, 1))

  f <- c(TRUE, TRUE, FALSE)
  expect_identical(split_on_dim(x, "axis2", f),
                   split_on_dim(x, 2L, f))

  expect_identical(split_along_dim(x, "axis2"),
                   split_along_dim(x, 2L))

  expect_identical(modify_along_dim(x, "axis2", function(x) x + 100),
                   modify_along_dim(x, 2L,      function(x) x + 100))

  xx <- rep_len(list(x), 5)
  expect_identical(bind_on_dim(xx, "axis2"),
                   bind_on_dim(xx, 2L))


})