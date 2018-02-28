


#' Split an array along a dimension
#'
#' `split_along_dim(X, .dim)` is equivalent to
#' `split_on_dim(X, seq_along_dim(X, .dim))`
#'
#' @param X an array, or list of arrays. Atomic vectors without a dimension
#'   attribute is treated as a 1 dimensions array. Names of list are preserved.
#' @param .dim a scalar integer, specifying which dimension to split along
#' @param f a vector or list of vectors. Must be the same length as the dimension being split. Passed on to `base::split()` (also, `base::interaction()` if a list).
#' @param drop passed on to `[`.
#' @param .keep_names Logical. If `TRUE` then if the dim being split along has
#'   dimnames, then the returned list has those names.
#'
#' @return A list of arrays, or if a list of arrays was passed in, then a list
#'   of lists of arrays.
#' @rdname split-array
#' @export
#'
#' @examples
#' X <- array(1:8, c(2,3,4))
#' X
#' split_along_dim(X, 2)
#' split_on_dim(X, 2, c("a", "a", "b"), drop = FALSE)
split_on_dim <- function(X, .dim,
                         f = dimnames(X)[[.dim]] %||% seq_along_dim(X, .dim),
                         drop = NULL) {

  if(is.list(f))
    f <- interaction(f, drop = TRUE)


  if (is.list(X) && is.null(dim(X)))
    lapply(X, function(x) split_on_dim(x, .dim, f = f, drop = drop))
  else {
    id <- seq_along_dim(X, .dim)
    if(!identical(length(id), length(f)))
      stop("`f` must be the same length as the dimension being split on.")
    l <- split(id, f)
    lapply(l, function(idx) extract_dim(X, .dim, idx, drop = drop))
  }
}

#' @rdname split-array
#' @export
split_on_rows <- function(X,
                          f = rownames(X) %||% seq_along_rows(X),
                          drop = NULL)
  split_on_dim(X, 1L, f = f, drop = drop)


#' @rdname split-array
#' @export
split_on_cols <- function(X,
                          f = colnames(X) %||% seq_along_cols(X),
                          drop = NULL)
  split_on_dim(X, 2L, f = f, drop = drop)




#' @rdname split-array
#' @export
split_along_dim <- function(X, .dim, drop = NULL, .keep_names = TRUE) {
  if (is.list(X) && is.null(dim(X))) # don't recurse on data.frame or other overloaded array-type classes
    return(lapply(X, function(x)
      split_along_dim(x, .dim, drop = drop, .keep_names = .keep_names)))


  out <- lapply(seq_along_dim(X, .dim),
                function(i)
                  extract_dim(X, .dim, i, drop = drop))

  if (isTRUE(.keep_names) && !is.null(nms <- dimnames(X)[[.dim]]))
    names(out) <- nms

  out
}

#' @rdname split-array
#' @export
split_along_rows <-
  function(X, drop = NULL, .keep_names = TRUE)
    split_along_dim(X, 1L, drop = drop, .keep_names = .keep_names)

#' @rdname split-array
#' @export
split_along_cols <-
  function(X, drop = NULL, .keep_names = TRUE)
    split_along_dim(X, 2L, drop = drop, .keep_names = .keep_names)

