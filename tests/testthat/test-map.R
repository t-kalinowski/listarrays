context("test-map.R")



test_that("map_along_dim", {
  A <- matrix2(letters[1:15], ncol = 3)

  for (d in seq_along(dim(A)))
    expect_identical(
      as.list(apply(A, 1, function(x) paste(x, collapse = ""))),
      map_along_rows(A, ~ paste(.x, collapse = "")))
})
