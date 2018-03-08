#' Split an array along a dimension
#'
#' `split_along_dim(X, .dim)` is equivalent to `split_on_dim(X, seq_along_dim(X,
#' .dim))`
#'
#' @param X an array, or list of arrays. Atomic vectors without a dimension
#'   attribute is treated as a 1 dimensions array. Names of list are preserved.
#' @param .dim a scalar integer, specifying which dimension to split along
#' @param f Specify how to split the dimension. \describe{
#'
#'    \item{character, integer or factor}{passed on to
#'   `base::split()`. Must be the same length as the dimention being split.}
#'
#'   \item{a list of vectors}{Passed on to `base::interaction()` then
#'   `base::split()`. Each vector in the list must be the same length as the
#'   dimention being split.}
#'
#'   \item{a scalar integer}{used to split into that many groups of equal size}
#'
#'   \item{a numeric vector where \code{all( f < 0 )} }{used to determin the relative
#'   proportions of the group being split. \code{sum(f)} must be \code{1}. For example
#'   \code{c(0.2, 0.2, 0.6)} will result approximatly a 20\%-20\%-60\% split.}
#' }
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
                         drop = NULL, depth = Inf) {

  if(is.list(f))
    f <- interaction(f, drop = TRUE)


  if (is.list(X) && is.null(dim(X)) && depth > 0L)
    return(lapply(X, function(x) split_on_dim(x, .dim, f = f, drop = drop, depth = depth - 1L)))


  if(.dim == 1 && dim(X)[1L] > 500L) {
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

  l <- split(id, f)
  lapply(l, function(idx)
    extract_dim(X, .dim, idx, drop = drop, depth = 0L))
}

#' @rdname split-array
#' @export
split_on_rows <- function(X,
                          f = rownames(X) %||% seq_along_rows(X),
                          drop = NULL, depth = Inf)
  split_on_dim(X, 1L, f = f, drop = drop, depth = depth)


#' @rdname split-array
#' @export
split_on_cols <- function(X,
                          f = colnames(X) %||% seq_along_cols(X),
                          drop = NULL, depth = Inf)
  split_on_dim(X, 2L, f = f, drop = drop, depth = depth)




#' @rdname split-array
#' @export
split_along_dim <- function(X, .dim, drop = NULL, .keep_names = TRUE, depth = Inf) {
  if (is.list(X) && is.null(dim(X)) && depth > 0L) # don't recurse on data.frame or other overloaded array-type classes
    return(lapply(X, function(x)
      split_along_dim(x, .dim, drop = drop, .keep_names = .keep_names, depth = depth - 1L)))


  if(.dim == 1 && length(dim(X)) >= 3L && dim(X)[1L] > 500L) {
    # subsetting on first index is oom slower than last index for large arrays
    # due to F style (column major) ordering of arrays.
    # aperm has a very fast strided slice in C
    X <- aperm(X, c(2:length(dim(X)), 1L))
    .dim <- length(dim(X))
  }

  expr <- extract_dim_chr_expr(X, .dim, .idx_var = "i", drop = drop)
  expr <- parse(text = expr)[[1]]

  # out <- vector("list", dim(X)[.dim])
  # # browser()
  #
  # for (i in seq_along_dim(X, .dim)) {
  #   # out[[i]] <- X[,,i]
  #   out[[i]] <- eval(expr)
  # }

  out <- lapply(seq_along_dim(X, .dim),
                function(i) eval(expr))


  # out <- lapply(seq_along_dim(X, .dim),
  #               function(i)
  #                 extract_dim(X, .dim, i, drop = drop, depth = 0L))
  #
  if (isTRUE(.keep_names) && !is.null(nms <- dimnames(X)[[.dim]]))
    names(out) <- nms

  out
}

#' @rdname split-array
#' @export
split_along_rows <-
  function(X, drop = NULL, .keep_names = TRUE, depth = Inf) {
    split_along_dim(X, 1L, drop = drop, .keep_names = .keep_names, depth = depth)
    # out <- unlist(apply(X, 1, list), recursive = FALSE)
    # lapply(out, drop_dimnames)
    # out
  }




# > dim(X)
# [1] 368000    128      2
# tl1 <- system.time(rl1 <- unlist(apply(X, 1, list), recursive = FALSE) %>% lapply(drop_dimnames))
# tl2 <- system.time(rl2 <-  split_along_rows(X))
# > identical(rl1, rl2)
# [1] TRUE
# > tl1
# user  system elapsed
# 3.027   0.297   3.301
# > tl2
# user  system elapsed
# 55.272   0.297  55.194

# it's because `[` is so slow for arrays
# Xa <- test$X
# Xm <- Xa
# dim(Xm) <- c(dim(X)[1], prod(dim(X)[-1]))
# tXm <- t(Xm)
# tXa <- Xa
# dim(tXa) <- rev(dim(Xa))
#
# library(microbenchmark)
# autoplot(microbenchmark(
#   Xa[6,,],
#   Xm[6,],
#   tXm[,6],
#   tXa[,,6]
# ))
# Unit: microseconds
# expr        min        lq    mean  median     uq    max neval cld
# Xa[6, , ]   3.317 3.6125 4.10827 3.8530 4.0535 16.036   100   c
# Xm[6, ]     1.460 1.5690 2.07374 1.8020 1.8850 16.223   100 a
# tXm[, 6]    1.490 1.5720 1.67813 1.6185 1.6955  2.747   100 a
# tXa[, , 6]  2.210 2.4320 2.62682 2.4965 2.6385  5.238   100  b
#
#
#apply() forces things into a matrix, then restores dim (maybe) after looping
#with f() on all elements


#' @rdname split-array
#' @export
split_along_cols <-
  function(X, drop = NULL, .keep_names = TRUE, depth = Inf)
    split_along_dim(X, 2L, drop = drop, .keep_names = .keep_names, depth = depth)

