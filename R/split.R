#' Split an array along a dimension
#'
#' `split_along_dim(X, .dim)` is equivalent to `split_on_dim(X, seq_along_dim(X,
#' .dim))`
#'
#' @param X an array, or list of arrays. Atomic vectors without a dimension
#'   attribute is treated as a 1 dimensions array (Meaning, atomic vectors
#'   without a dim attribute are only accepted if `.dim` is `1`. Names of list
#'   are preserved. If a list of arrays, all the arrays must have the same
#'   length of the dimension being split.
#' @param .dim a scalar integer, specifying which dimension to split along
#' @param f Specify how to split the dimension. \describe{
#'
#'   \item{character, integer or factor}{passed on to `base::split()`. Must be
#'   the same length as the dimention being split.}
#'
#'   \item{a list of vectors}{Passed on to `base::interaction()` then
#'   `base::split()`. Each vector in the list must be the same length as the
#'   dimention being split.}
#'
#'   \item{a scalar integer}{used to split into that many groups of equal size}
#'
#'   \item{a numeric vector where \code{all( f < 0 )} }{used to determin the
#'   relative proportions of the group being split. \code{sum(f)} must be
#'   \code{1}. For example \code{c(0.2, 0.2, 0.6)} will return approximatly a
#'   20\%-20\%-60\% split.} }
#' @param drop passed on to `[`.
#' @param .keep_names Logical. If `TRUE` then if the dim being split along has
#'   dimnames, then the returned list has those names.
#' @param depth Scalar number, how many levels to recurse down. Set this if you
#'   want to explicit treat a list as a vector (that is, a one-dimentional
#'   array). (You can alternatively set dim attributes with `dim<-` on the list
#'   to prevent recursion)
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
                         drop = FALSE, depth = Inf) {

  if(is.list(f))
    f <- interaction(f, drop = TRUE)

  if (is.list(X) && is.null(dim(X)) && depth > 0L)
    return(lapply(X, function(x) split_on_dim(x, .dim, f = f, drop = drop, depth = depth - 1L)))

  if (is.character(.dim))
    .dim <- match(.dim, names(dimnames(X)))

  if (.dim == 1L && length(dim(X)) >= 3L &&
      dim(X)[1L] >= 1e5L && any(dim(X)[-1L] != 1L)) {
    # subsetting on first index is OOM slower than on last index for large arrays
    # due to F style (column major) ordering of arrays. aperm() has a very fast
    # strided slice written in C, and for large arrays it makes sense to do this
    # upfront.
    X <- aperm(X, c(2:length(dim(X)), 1L))
    .dim <- length(dim(X))
  }

  id <- seq_along_dim(X, .dim)

  if(is.scalar.integerish(f))
    f <- cut(id, f, labels = paste0("grp", seq_len(f)))
  else if (all(f < 1)) {
    stopifnot(sum(f) == 1)
    f <- cut(id, c(0, cumsum(f) * length(id)),
          labels = paste0("grp", seq_along(f)))
  }


  if (!identical(length(id), length(f)))
    stop("`f` must be the same length as the dimension being split on.")

  expr <- extract_dim_chr_expr(X, .dim, .idx_var = "idx", drop = drop)
  expr <- parse(text = expr, keep.source = FALSE)[[1]]

  l <- split(id, f)
  # lapply(l, function(idx) eval(expr))

  out <- vector("list", length(l))
  for(i in seq_along(l)) {
    idx <- l[[i]]
    out[[i]] <- eval(expr)
  }

  names(out) <- names(l)
  out
}

#' @rdname split-array
#' @export
split_on_rows <- function(X,
                          f = rownames(X) %||% seq_along_rows(X),
                          drop = FALSE, depth = Inf)
  split_on_dim(X, 1L, f = f, drop = drop, depth = depth)


# ' @rdname split-array
# ' @export
# split_on_cols <- function(X,
#                           f = colnames(X) %||% seq_along_cols(X),
#                           drop = NULL, depth = Inf)
#   split_on_dim(X, 2L, f = f, drop = drop, depth = depth)




#' @rdname split-array
#' @export
split_along_dim <- function(X, .dim, drop = NULL, .keep_names = TRUE, depth = Inf) {
  if (is.list(X) && is.null(dim(X)) && depth > 0L) # don't recurse on data.frame or other overloaded array-type classes
    return(lapply(X, function(x)
      split_along_dim(x, .dim, drop = drop, .keep_names = .keep_names, depth = depth - 1L)))


  if (is.character(.dim))
    .dim <- match(.dim, names(dimnames(X)))


  if (.dim == 1L && length(dim(X)) >= 3L &&
      dim(X)[1L] >= 1e5L && any(dim(X)[-1L] != 1L)) {
    # subsetting on first index is OOM slower than on last index for large arrays
    # due to F style (column major) ordering of arrays. aperm() has a very fast
    # strided slice written in C, and for large arrays it makes sense to do this
    # upfront.
    X <- aperm(X, c(2:length(dim(X)), 1L))
    .dim <- length(dim(X))
  }

  expr <- extract_dim_chr_expr(X, .dim, .idx_var = "i",
                               drop = drop, .var_to_subset = "X")

  expr <- parse(text = expr, keep.source = FALSE)[[1]]

  # out <- lapply(seq_along_dim(X, .dim), function(i, Xin) eval(expr), Xin = X)

  out <- vector("list", get_dim(X)[.dim])
  for (i in seq_along_dim(X, .dim))
    out[[i]] <- eval(expr)


  if (isTRUE(.keep_names) && !is.null(nms <- dimnames(X)[[.dim]]))
    names(out) <- nms

  out
}


#' @rdname split-array
#' @export
split_along_rows <-
  function(X, drop = NULL, .keep_names = TRUE, depth = Inf)
    split_along_dim(X, 1L, drop = drop, .keep_names = .keep_names, depth = depth)



# ' @rdname split-array
# ' @export
# split_along_cols <-
#   function(X, drop = NULL, .keep_names = TRUE, depth = Inf)
#     split_along_dim(X, 2L, drop = drop, .keep_names = .keep_names, depth = depth)



