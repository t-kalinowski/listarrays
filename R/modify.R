


#' Modify an array by mapping over 1 or more dimensions
#'
#' This function can be thought of as a version of `base::apply()` that is
#' guaranteed to return a object of the same dimensions as it was input. It also
#' generally preserves attributes, as it's built on top of `[<-`.
#'
#' @param X An array, or a list of arrays
#' @param which_dim integer vector of dimensions to modify at
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
modify_along_dim <- function(X, which_dim, .f, ...) {

  if(is.list(X) && is.null(dim(X)))
    return(lapply(X, function(x) modify_along_dim(x, which_dim, .f, ...)))


  if (requireNamespace("rlang", quietly = TRUE)) {
    .f <- rlang::as_function(.f)
  } else {
    if (inherits(.f, "formula"))
      stop("Specifing functions via forumla syntax requires package rlang to be available")
    .f <- match.fun(.f)
  }

  which_dim <- standardize_which_dim(which_dim, X, multiple_OK = TRUE)

  names(which_dim) <- paste0("idx", seq_along(which_dim))

  Xe <- extract_dim_chr_expr(X, which_dim,
                             .idx_var = names(which_dim),
                             .var_to_subset = "X")

  oXe <- paste0("o", Xe)

  loop_body <- paste0(Xe, " <- .f(", oXe, ", ...)")

  loop_controlflow <- paste0(collapse = "\n",
    "for (", names(which_dim), " in .seq_along_dim(X,", which_dim, "))")

  loop <- paste0(loop_controlflow, "\n   ", loop_body)

  args <- as.pairlist(alist(X = , .f = , ... = ))
  body <- parse1("{
      oX <- X
      storage.mode(X) <- 'logical'
      ", loop, "
      X
  }")

  modify_it <- eval(call("function", args, body))

  if (prod(which_dim) > 100)
    modify_it <- cmpfun(modify_it)

  modify_it(X,  .f, ...)
}

#' @export
#' @rdname modify_along_dim
modify_along_rows <- function(X, .f, ...)
  modify_along_dim(X, 1L, .f, ...)

#' @export
#' @rdname modify_along_dim
modify_along_cols <- function(X, .f, ...)
  modify_along_dim(X, -1L, .f, ...)


