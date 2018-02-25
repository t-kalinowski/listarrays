


#' Drop dimnames
#'
#' A pipe-friendly wrapper for `dimnames(x) <- NULL` or
#' \code{dimnames(x)[[.dim]] <- NULL}
#'
#'
#' @param x an object, potentially with dimnames
#' @param .dim If `NULL` (the default) then all dimnames are dropped. If integer
#'   vector, then dimnames only at the specified dimensions are dropped.
#'
#' @export
drop_dimnames <- function(x, .dim = NULL) {
  if(is.null(.dim))
    dimnames(x) <- NULL
  else
    for(d in .dim)
      dimnames(x)[[d]] <- NULL

  x
}

#' @rdname drop_dimnames
#' @export
drop_rownames <- function(x) {
  rownames(x) <- NULL
  x
}

#' @rdname drop_dimnames
#' @export
drop_colnames <- function(x) {
  colnames(x) <- NULL
  x
}

