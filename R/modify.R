


#' Modify an array by mapping over 1 or more dimensions
#'
#' @param X An array, or a list of arrays
#' @param .dim integer vector of dimensions to modify at
#' @param .f a function or formula defining a function(same semantics as
#'   [`purrr::map`]). The function must return either an array the same shape as
#'   it was passed, a vector of the same length, or a scalar, although the type
#'   of the returned object does not need to be the same as was passed in.
#' @param ... passed on to `.f()`
#'
#' @export
#' @rdname modify_along_dim
#' @return An array, or if `X` was a list, a list of arrays of the same shape as
#'   was passed in.
#' @examples
#' x <- array(1:6, 1:3)
#' modify_along_dim(x, 3, ~mean(.x))
#' modify_along_dim(x, 3, ~.x/mean(.x))
modify_along_dim <- function(X, .dim, .f, ...) {

  if(is.list(X) && is.null(dim(X)))
    return(lapply(X, function(x) modify_along_dim(x, .dim, .f, ...)))


  if(inherits(.f, "formula")) {
    if( !requireNamespace("rlang") )
      stop("Specifing functions via forumla syntax requires package rlang to be available")

    .f <- rlang::as_function(.f)
  } else
    .f <- match.fun(.f)


  if(is.character(.dim))
    .dim <- match(.dim, names(get_dim(X)))

  stopifnot(is.integerish(.dim))

  names(.dim) <- paste0("idx", seq_along(.dim))

  Xe <- extract_dim_chr_expr( X, .dim,
                              .idx_var = paste0("combs$", names(.dim), "[[r]]"),
                              .var_to_subset = "X")

  oXe <- paste0("o", Xe)
  oX <- X

  X <- array(dim = dim(X), dimnames = dimnames(X))
  # allow for different return types,
  # e.g, double in -> integer out

  expr <- parse(text = paste0(Xe, " <- .f(", oXe, ", ...)"))[[1]]

  # consider replacing this with an iterator from package:arrangements
  # or a hand-crafted generator function
  dims <- dim(X)[.dim]
  names(dims) <- names(.dim)

  combs <- do.call(
    function(...) expand.grid(..., KEEP.OUT.ATTRS = FALSE),
    lapply(dims, seq_len)
  )

  for (r in seq_along_rows(combs))
    eval(expr)

  X
}


#' @export
#' @rdname modify_along_dim
modify_along_rows <- function(X, .f, ...)
  modify_along_dim(X, 1L, .f, ...)

#' @export
#' @rdname modify_along_dim
modify_along_cols <- function(X, .f, ...)
  modify_along_dim(X, 2L, .f, ...)



## This is with new, apply-based version of split_along_rows
# > dim(Y)
# [1] 368000     23
# > t1 <- system.time(r1 <- t(apply(Y, 1, sort.list, decreasing = TRUE)))
# > t2 <- system.time(r2 <- modify_along_rows(Y, sort.list, decreasing = TRUE))
# > t3 <- system.time(r3 <- split_along_rows(Y) %>% lapply(sort.list, decreasing = TRUE) %>% bind_as_rows)
# > identical(r1, r2)
# [1] TRUE
# > identical(r1, r3)
# [1] TRUE
# > str(r1)
# int [1:368000, 1:23] 7 5 15 20 8 16 12 20 18 14 ...
# > t1
# user  system elapsed
# 7.269   0.035   7.255
# > t2
# user  system elapsed
# 12.166   0.031  12.114
# > t3
# user  system elapsed
# 10.236   0.058  10.227