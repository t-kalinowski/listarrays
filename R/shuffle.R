
#' Shuffle along the first dimension multiple arrays in sync
#'
#' @param ... arrays of various dimensions (vectors and data.frames OK too)
#'
#' @return A list of objects passed on to `...`, or if a single object was
#'   supplied, then the single object shuffled
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
shuffle_rows <- function(...) {
  l <- list(...)

  single_obj_in <- identical(length(l), 1L)
  single_list_in <- is.list(l[[1]]) && is.null(dim(l[[1]]))

  if(single_list_in) {
    single_obj_in <- FALSE
    l <- l[[1L]]
  }

  nrows <- unique(vapply(l, NROW, 0L))
  if(!identical(length(nrows), 1L))
    stop("All objects passed to `...` must have the same number of rows")

  idx <- sample.int(nrows)

  for (i in seq_along(l))
    l[[i]] <- extract_rows(l[[i]], idx, drop = FALSE)

  if (single_obj_in)
     l[[1L]]
  else
    l
}
