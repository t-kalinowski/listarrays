context("test-bind.R")

test_that("bind arrays works", {

  lx <- replicate(10, array(1:8, 2:4), FALSE)

  for(d in 1:3)
    expect_equal(dim(bind_as_dim(lx, d))[d], 10L)

  for(d in 1:3)
    expect_equal(dim(bind_on_dim(lx, d))[d], 10L*dim(lx[[1]])[d])

  # dnn <- dimnames(provideDimnames(lx[[1]]))
  # dnn <- lapply(seq_along(dnn), function(i) {
  #   paste(i, dnn[[i]], sep = "_")
  # })
  dnn <- list(c("1_A", "1_B"),
              c("2_A", "2_B", "2_C"),
              c("3_A", "3_B", "3_C", "3_D"))

  lx <- lapply(lx, function(x) {
    dimnames(x) <- dnn
    x
  })

  new_dnn <- dimnames(bind_as_rows(lx))
  expect_identical(new_dnn, c(list(NULL), dnn))

  names(lx) <- paste0("newdim_", 1:10)
  new_dnn <- dimnames(bind_as_rows(lx))
  expect_identical(new_dnn, c(list(names(lx)), dnn))

})
