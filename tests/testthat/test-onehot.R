context("test-onehot.R")

test_that("onehot encoding and decoding", {

  y <- letters[1:4]
  Y <- matrix(c(1, 0, 0, 0,
                0, 1, 0, 0,
                0, 0, 1, 0,
                0, 0, 0, 1),
              byrow = TRUE, ncol = 4)
  colnames(Y) <- y



  expect_identical(Y, onehot(y))
  expect_identical(y, decode_onehot(Y))

  decode <- onehot_with_decoder(y)[[2]]
  expect_identical(y, decode(Y))


})
