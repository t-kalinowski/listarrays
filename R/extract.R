#' Extract with `[` on a specified dimension
#'
#' @param X Typically, an array, but any object with a `[` method is accepted
#'   (e.g., dataframe, vectors)
#' @param which_dim A scalar integer or character, specifying the dimension to extract from
#' @param idx A numeric, boolean, or character vector to perform subsetting with.
#' @param drop Passed on to `[`. If `NULL` (the default), then drop is omitted
#'   from the argument, and the default is used (defaults to TRUE for most
#'   objects, including arrays)
#' @param depth Scalar number, how many levels to recurse down if `X` is a list
#'   of arrays. Set this if you want to explicit treat a list as a vector (that
#'   is, a one-dimensional array). (You can alternatively set a dim attribute
#'   with `dim<-` on the list to prevent recursion)
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
extract_dim <- function(X, which_dim, idx, drop = NULL, depth = Inf) {
  which_dim <- standardize_which_dim(which_dim, X)

  if(is.list(X) && is.null(dim(X)) && depth > 0L)
    return(lapply(X, function(x)
      extract_dim(x, which_dim, idx, drop = drop, depth = depth - 1L)))

  expr <- extract_dim_chr_expr(X, which_dim, idx_var_nm = "idx", drop = drop)
  expr <- parse(text = expr, keep.source = FALSE)[[1]]
  eval(expr)
}


#' @rdname extract_dim
#' @export
extract_rows <- function(X, idx, drop = NULL, depth = Inf)
  extract_dim(X, 1L, idx, drop = drop, depth = depth)

#' @rdname extract_dim
#' @export
extract_cols <- function(X, idx, drop = NULL, depth = Inf)
  extract_dim(X, -1L, idx, drop = drop, depth = depth)




extract_dim_chr_expr <-
  function(X, which_dim, drop = NULL, ndims = ndim(X),
           idx_var_nm = names(which_dim) %||%
             paste0("idx", if(length(which_dim) > 1L) seq_along(which_dim)),
           var_to_subset = deparse(substitute(X))) {

    force(var_to_subset)

    which_dim <- as.integer(which_dim)
    stopifnot(identical(length(idx_var_nm), length(which_dim)),
              !anyDuplicated(idx_var_nm))

    args <- character(ndims)
    args[which_dim] <- idx_var_nm

    if(!is.null(drop))
      args <- c(args, " drop = drop")

    args <- paste0(args, collapse = ",")
    sprintf("%s[%s]", var_to_subset, args)
  }
