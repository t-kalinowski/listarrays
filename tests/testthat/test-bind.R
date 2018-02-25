context("test-bind.R")

test_that("bind arrays works", {

  lx <- replicate(10, array(1:8, 2:4), FALSE)

  for(d in 1:3)
    expect_equal(dim(bind_as_dim(lx, d))[d], 10L)

  for(d in 1:3)
    expect_equal(dim(bind_on_dim(lx, d))[d], 10L*dim(lx[[1]])[d])
})
