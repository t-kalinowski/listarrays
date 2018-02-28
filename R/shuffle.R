


#' Shuffle along the first dimension multiple arrays in sync
#'
#' @param ... arrays of various dimensions (vectors and data.frames OK too)
#' @param in_sync if the objects should be shuffled in sync (i.e., row order is
#'   the same in all returned objects)
#'
#' @return A list of objects passed on to `...`
#' @export
#'
#' @examples
#' x <- 1:3
#' y <- matrix(1:9, ncol = 3)
#' z <- array(1:27, c(3,3,3))
#'
#' if(require(zeallot)) {
#'   c(xs, ys, zs) %<-% shuffle_rows(x, y, z)
#'
#'   l <- lapply(seq_along_rows(y), function(r) {
#'     list(x = x[r], y = y[r,], z = z[r,,])
#'   })
#'
#'   ls <- lapply(seq_along_rows(y), function(r) {
#'     list(x = xs[r], y = ys[r,], z = zs[r,,])
#'   })
#'
#'   stopifnot(
#'     length(unique(c(l, ls))) == length(l))
#' }
shuffle_rows <- function(..., in_sync = TRUE) {
  l <- list(...)
  if(is.list(l[[1]]) && identical(length(l), 1L))
    l <- l[[1]]

  n <- length(l)

  n_cases <- unique(lapply(l, robust_nrow)) # nrow, or if 1d vec length
  stopifnot(length(n_cases) == 1L)
  n_cases <- n_cases[[1]]

  if(in_sync)
    idx <- sample.int(n_cases)
  else
    makeActiveBinding("idx", function() sample.int(n_cases), environment())

  for (i in seq_along(l))
    l[[i]] <- extract_rows(l[[i]], idx, drop = FALSE)

  l
}
