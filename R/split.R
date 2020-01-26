#' Split an array along a dimension
#'
#' @param X an array, or list of arrays. An atomic vector without a dimension
#'   attribute is treated as a 1 dimensional array (Meaning, atomic vectors
#'   without a dim attribute are only accepted if `which_dim` is `1`. Names of
#'   the passed list are preserved. If a list of arrays, all the arrays must
#'   have the same length of the dimension being split.
#' @param which_dim a scalar string or integer, specifying which dimension to
#'   split along. Negative integers count from the back. If a string, it must
#'   refer to a named dimension (e.g, one of `names(dimnames(X))`.
#' @param f Specify how to split the dimension. \describe{
#'
#'   \item{character, integer, factor}{passed on to `base::split()`. Must be the
#'   same length as the dimension being split.}
#'
#'   \item{a list of vectors}{Passed on to `base::interaction()` then
#'   `base::split()`. Each vector in the list must be the same length as the
#'   dimension being split.}
#'
#'   \item{a scalar integer}{used to split into that many groups of equal size}
#'
#'   \item{a numeric vector where \code{all(f<0)}}{specifies the relative size
#'   proportions of the groups being split. \code{sum(f)} must be \code{1}. For
#'   example \code{c(0.2, 0.2, 0.6)} will return approximately a 20\%-20\%-60\%
#'   split.} }
#' @param drop passed on to `[`.
#' @param depth Scalar number, how many levels to recurse down. Set this if you
#'   want to explicitly treat a list as a vector (that is, a one-dimensional
#'   array). (You can alternatively set dim attributes with `dim<-` on the list
#'   to prevent recursion)
#'
#'   `split_along_dim(X, which_dim)` is equivalent to `split_on_dim(X,
#'   which_dim, seq_along_dim(X, which_dim))`.
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
#'
#' # specify f as a factor, akin to base::split()
#' split_on_dim(X, 2, c("a", "a", "b"), drop = FALSE)
#'
#' d <- c(10, 3, 3)
#' X <- array(1:prod(d), d)
#' y <- letters[1:10]
#' Y <- onehot(y)
#'
#' # specify `f`` as relative partition sizes
#' if(require(zeallot) && require(magrittr) && require(purrr)) {
#'
#' c(train, validate, test) %<-% {
#'   list(X = X, Y = Y, y = y) %>%
#'     shuffle_rows() %>%
#'     split_on_rows(c(0.6, 0.2, 0.2)) %>%
#'     transpose()
#' }
#'
#' str(test)
#' str(train)
#' str(validate)
#'
#' }
#'
#'
#' # with with array data in a data frame by splitting row-wise
#' if(require(tibble))
#'   tibble(y, X = split_along_rows(X))
split_on_dim <- function(X, which_dim,
                         f = dimnames(X)[[which_dim]],
                         drop = FALSE, depth = Inf) {

  stopifnot(!is.null(f))

  if(is.list(f))
    f <- interaction(f, drop = TRUE)

  if (is.list(X) && is.null(dim(X)) && depth > 0L)
    return(lapply(X, function(x)
      split_on_dim(x, which_dim, f = f, drop = drop, depth = depth - 1L)))

  which_dim <- standardize_which_dim(which_dim, X)

  id <- .seq_along_dim(X, which_dim)

  if(is.scalar.integerish(f))
    f <- cut(id, f, labels = paste0("grp", seq_len(f)))
  else if (is.numeric(f) && all(f < 1)) {
    stopifnot(sum(f) == 1)
    f <- cut(id, c(0, cumsum(f) * length(id)),
             labels = names(f) %||% paste0("grp", seq_along(f)))
  }

  if (!identical(length(id), length(f)))
    stop("`f` must be the same length as the dimension being split on.")

  l <- split(id, f)

  extract_call <- extract_dim_expr(X, which_dim,
                                   idx_var_sym = quote(l[[i]]), drop = drop)
  split_it <- new_split_on_fn(extract_call)

  if(length(l) > 5000)
    split_it <- cmpfun(split_it)

  out <- split_it(X, l)

  # names(out) <- names(l)
  out
}

SPLIT_ON_FN_TEMPLATE <- alist(X = , l = , {
  out <- vector("list", length(l))
  for (i in seq_along(l))
    out[[i]] <- EXTRACT_EXPR
  out
})
new_split_on_fn <- function(extract_expr) {
  SPLIT_ON_FN_TEMPLATE[[c(3, 3, 4, 3)]] <- extract_expr
}

SPLIT_ON_FN_TEMPLATE <- alist(X = , l = , {
  for (i in seq_along(l))
    l[[i]] <- EXTRACT_EXPR
  l
})

new_split_on_fn <- function(extract_expr) {
  SPLIT_ON_FN_TEMPLATE[[c(3, 2, 4, 3)]] <- extract_expr
  as.function.default(SPLIT_ON_FN_TEMPLATE, envir = minimal_split_along_fn_env)
}



#' @rdname split-array
#' @export
split_on_rows <- function(X,
                          f = rownames(X),
                          drop = FALSE, depth = Inf)
  split_on_dim(X, 1L, f = f, drop = drop, depth = depth)

#' @rdname split-array
#' @export
split_on_cols <- function(X,
                          f = rownames(X),
                          drop = FALSE, depth = Inf)
  split_on_dim(X, -1L, f = f, drop = drop, depth = depth)







minimal_split_along_fn_env <- as.environment(list(
  `<-` = `<-`,
  `{` = `{`,
  `[` = `[`,
  `[[<-` = `[[<-`,
  `[[` = `[[`,
  vector = vector,
  `for` = `for`,
  seq_len = seq_len,
  seq_along = seq_along
))


SPLIT_ALONG_FN_TEMPLATE <-
  alist(X = , length_out = , {
    out <- vector('list', length_out)
    for (i in seq_len(length_out))
      out[[i]] <-  EXTRACT_CALL
    out
  })

SPLIT_ALONG_FN_TEMPLATE <-
  alist(X = , length_out = , {
    out <- vector('list', LENGTH_OUT)
    for (i in seq_len(LENGTH_OUT))
      out[[i]] <-  EXTRACT_CALL
    out
  })

fn1 <- function() {
  eval(substitute(alist(X = , {
    out <- vector('list', LENGTH_OUT)
    for (i in seq_len(LENGTH_OUT))
      out[[i]] <-  EXTRACT_CALL
    out
  }), list(LENGTH_OUT = 11L)))
}

SPLIT_ALONG_FN_TEMPLATE <- alist(X = , {
  out <- vector('list', LENGTH_OUT)
  for (i in seq_len(LENGTH_OUT))
    out[[i]] <-  EXTRACT_CALL
  out
})


new_split_along_fn <- function(extract_call, length_out) {
  SPLIT_ALONG_FN_TEMPLATE[[c(2L, 3L, 4L, 3L)]] <- extract_call
  SPLIT_ALONG_FN_TEMPLATE[[c(2L, 2L, 3L, 3L)]] <- length_out
  SPLIT_ALONG_FN_TEMPLATE[[c(2L, 3L, 3L, 2L)]] <- length_out

  as.function.default(SPLIT_ALONG_FN_TEMPLATE,
                      envir = minimal_split_along_fn_env)
}

#' @rdname split-array
#' @export
#' @importFrom compiler cmpfun
split_along_dim <- function(X, which_dim, depth = Inf) {

  # don't recurse on data.frame or other overloaded array-type classes
  if (is.list(X) && is.null(dim(X)) && depth > 0L)
    return(lapply(X, function(x)
      split_along_dim(x, which_dim, depth = depth - 1L)))

  which_dim <- standardize_which_dim(which_dim, X, multiple_OK = TRUE)
  if(ndim(X) == 1L)
    X <- expand_dims(X)
  X <- as.array(X)

  # TODO: consider adding support for `drop = TRUE` old behavior
  X <- asplit(as.array(X), which_dim)
  X
}



#' @rdname split-array
#' @export
split_along_rows <- function(X, depth = Inf) {
  if (is.list(X) && is.null(dim(X)) && depth > 0L)
    return(lapply(X, function(x)
      split_along_rows(x, depth = depth - 1L)))
  .split_along_rows(X)
}
  # split_along_dim(X, 1L, depth = depth)

#' @rdname split-array
#' @export
split_along_cols <- function(X, depth = Inf)
  split_along_dim(X, -1L, depth = depth)


# TODO:
as_listarray <- function() {}
unlist.listarray <- function() {}


