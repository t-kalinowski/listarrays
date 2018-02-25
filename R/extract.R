



#' Extract with `[` on a specified dimension
#'
#' @param X Typically, an array, but any object with a `[` method is accepted
#'   (e.g., dataframe, vectors)
#' @param .dim A scalar integer, specifying the dimensions to extract from
#' @param idx A numeric, boolean, or character vector to perform subsetting with
#' @param drop Passed on to `[`. If `NULL` (the default), then drop is omitted
#'   from the argument, and the default is used (defaults to TRUE for most
#'   objects, including arrays)
#'
#' @export
#'
#' @examples
#' # extract_rows is useful to keep the same code path for arrays of various sizes
#' X <- array(1:8, c(4, 3, 2))
#' y <- c("a", "b", "c", "d")
#' (Y <- onehot(y))
#'
#' extract_rows(X, 2)
#' extract_rows(Y, 2)
#' extract_rows(y, 2)
#'
#' library(zeallot)
#' c(X2, Y2, y2) %<-% extract_rows(list(X, Y, y), 2)
#' X2
#' Y2
#' y2
extract_dim <- function(X, .dim, idx, drop = NULL) {
  stopifnot(is.scalar.integerish(.dim))

  if(is.list(X))
    return(lapply(X, function(x) extract_dim(x, .dim, idx, drop = drop)))

  expr <- extract_dim_chr_expr(X, .dim,  drop = drop)
  eval(parse(text = expr)[[1]])
}


#' @rdname extract_dim
#' @export
extract_rows <- function(X, idx, drop = NULL)
  extract_dim(X, 1L, idx, drop = drop)

#' @rdname extract_dim
#' @export
extract_cols <- function(X, idx, drop = NULL)
  extract_dim(X, 2L, idx, drop = drop)




extract_dim_chr_expr <- function(X, .dim, drop = NULL,
                                     .ndims = ndims(X),
                                     .idx_var = names(.dim) %||% paste0("idx", if(length(.dim) > 1L) .dim),
                                     .var_to_subset = deparse(substitute(X))) {
  force(.var_to_subset)

  .dim <- as.integer(.dim)
  stopifnot(identical(length(.idx_var), length(.dim)), !anyDuplicated(.idx_var))

  args <- character(.ndims)
  args[.dim] <- .idx_var

  if(!is.null(drop))
    args <- c(args, " drop = drop")

  args <- paste0(args, collapse = ",")
  paste0(.var_to_subset, "[", args, "]")
}

