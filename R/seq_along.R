#' Sequence along a dimension
#'
#' @param x a dataframe, array or vector. For `seq_along_rows`, and
#'   `seq_along_cols` sequence along the first and second dimensions,
#'   respectively. Objects supplied are coerced with `as.array` prior to
#'   determining dimensions, so atomic vectors are treated as 1 dimensional
#'   arrays (i.e., `seq_along_rows` is equivalent to `seq_along` when `x` is an
#'   atomic vector or list).
#' @param .dim a scalar integer, specifying which dimension to generate a sequence for.
#'
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
# seq_along_dim <- function(x, .dim)
#   seq_len(get_dim(x)[[.dim]])


seq_along_dim <- function(x, which_dim)
  seq_len( dim(x)[[standardize_which_dim(x, which_dim)]] )

.seq_along_dim <- function(x, which_dim)
  seq_len( dim(x)[[which_dim]] )




get_dim <- function(x) {
  d <- dim(x) %||% length(x)
  if(!is.null(dnn <- names(dimnames(x))))
    names(d) <- dnn
  d
}



#' @rdname seq_along_dim
#' @export
seq_along_rows <- function(x) seq_along_dim(x, 1L)

# ' @rdname seq_along_dim
# ' @export
seq_along_cols <- function(x) seq_along_dim(x, -1L)








#' #' reverse order along a dimension
#' #'
# ' #' @param x a dataframe, array, or vector, or anything with a `[` method
#' #'   defined. In the case of a vector, this is equivalent to `rev`
# ' #' @param .dim a scalar integer. Which dimension to reverse.
#' #'
# ' #' @export
#' reverse_dim <- function(x, .dim) {
#'   if(dim_len <- robust_dim(x)[[.dim]])
#'     extract_dim(x, .dim, dim_len:1L, drop = FALSE)
#'   else
#'     x
#' }
#'
# ' #' @rdname reverse_dim
# ' #' @export
#' reverse_rows <- function(x) {
#'   if(nrows <- robust_nrow(x))
#'     extract_rows(x, nrows:1L, drop = FALSE)
#'   else
#'     x
#' }
#'
#'
# ' #' @export
# ' #' @rdname reverse_dim
#' reverse_cols <- function(x) {
#'   # not sure what the intended behavior should be for vectors or 1 dimensional arrays
#'   if(ncols <- robust_ncol(x))
#'     extract_cols(x, ncols:1L, drop = FALSE)
#'   else
#'     x
#' }


