


#' Sequence along a dimension
#'
#' @param x a dataframe, array or vector. For `seq_along_rows`, and
#'   `seq_along_cols` sequence along the first and second dimensions,
#'   respectively. Objects supplied are coerced with `as.array` prior to
#'   determining dimensions, so atomic vectors are treated as 1 dimensional
#'   arrays (i.e., `seq_along_rows` is equivalent to `seq_along` when `x` is an
#'   atomic vector or list).
#'
#'
#' @return a vector of integers 1:nrow(x), safe for use in `for` loops and
#'   vectorized equivalents.
#' @export
#' @family array-helpers
#' @examples
#' for (r in seq_along_rows(mtcars[1:4,]))
#'   print(mtcars[r,])
#'
#' x <- 1:3
#' identical(row_along(x), seq_along(x))
#'
#' @export
seq_along_dim <- function(x, .dim) {
  d <- dim(x) %||% dim(as.array(x))
  seq_len( d[[.dim]] )
}

#' @rdname seq_along_dim
#' @export
seq_along_rows <- function(x) seq_along_dim(x, 1L)

#' @rdname seq_along_dim
#' @export
seq_along_cols <- function(x) seq_along_dim(x, 2L)

# seq_len(robust_nrow(x))

# ' @rdname seq_along_dim
# ' @export
# '  `row_along` is an alias for `seq_along_rows`.
# row_along <- seq_along_rows






#' reverse order along a dimension
#'
#' @param x a dataframe, array, or vector, or anything with a `[` method
#'   defined. In the case of a vector, this is equivalent to `rev`
#'
#' @export
reverse_dim <- function(x, .dim) {
  if(dim_len <- robust_dim(x)[[.dim]])
    extract_dim(x, .dim, dim_len:1L, drop = FALSE)
  else
    x
}

#' @rdname reverse_dim
#' @export
reverse_rows <- function(x) {
  if(nrows <- robust_nrow(x))
    extract_rows(x, nrows:1L, drop = FALSE)
  else
    x
}


#' @export
#' @rdname reverse_dim
reverse_cols <- function(x) {
  # not sure what the intended behavior should be for vectors or 1 dimensional arrays
  if(ncols <- robust_ncol(x))
    extract_cols(x, ncols:1L, drop = FALSE)
  else
    x
}


