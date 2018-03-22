


#' Modify an array by mapping over 1 or more dimensions
#'
#' This function can be thought of as a version of `base::apply()` that is
#' guaranteed to return a object of the same dimensions as it was input. It also
#' generally preserves attributes, as it's built on `[<-` and `[`.
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
    if( !requireNamespace("rlang", quietly = TRUE))
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

  expr <- parse1(paste0(Xe, " <- .f(", oXe, ", ...)"))

  # consider replacing this with an iterator from package:arrangements
  # or a hand-crafted generator function
  dims <- dim(X)[.dim]
  names(dims) <- names(.dim)

  combs <- do.call(
    function(...) expand.grid(..., KEEP.OUT.ATTRS = FALSE),
    lapply(dims, seq_len)
  )

  e <- environment()
  eval <- maybe_eval_bare()

  for (r in seq_along_rows(combs))
    eval(expr, e)

  X
}


#' @export
#' @rdname modify_along_dim
modify_along_rows <- function(X, .f, ...)
  modify_along_dim(X, 1L, .f, ...)

# ' @export
# ' @rdname modify_along_dim
# modify_along_cols <- function(X, .f, ...)
#   modify_along_dim(X, 2L, .f, ...)


