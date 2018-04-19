#' Sequence along a dimension
#'
#' @param x a dataframe, array or vector. For `seq_along_rows`, and
#'   `seq_along_cols` sequence along the first and second dimensions,
#'   respectively. Objects supplied are coerced with `as.array` prior to
#'   determining dimensions, so atomic vectors are treated as 1 dimensional
#'   arrays (i.e., `seq_along_rows` is equivalent to `seq_along` when `x` is an
#'   atomic vector or list).
#' @param which_dim a scalar integer or character string, specifying which
#'   dimension to generate a sequence for. Negative numbers count from the back
#'
#' @return a vector of integers 1:nrow(x), safe for use in `for` loops and
#'   vectorized equivalents.
#' @export
#' @examples
#' for (r in seq_along_rows(mtcars[1:4,]))
#'   print(mtcars[r,])
#'
#' x <- 1:3
#' identical(seq_along_rows(x), seq_along(x))
#'
#' @export
seq_along_dim <- function(x, which_dim)
  seq_len( DIM(x)[[standardize_which_dim(which_dim, x)]] )

.seq_along_dim <- function(x, which_dim)
  seq_len( DIM(x)[[which_dim]] )


#' @rdname seq_along_dim
#' @export
seq_along_rows <- function(x) seq_along_dim(x, 1L)

#' @rdname seq_along_dim
#' @export
seq_along_cols <- function(x) seq_along_dim(x, -1L)
