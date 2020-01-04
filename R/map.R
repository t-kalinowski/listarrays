#' Apply a function across subsets along an array dimension
#'
#' @description
#' `map_along_dim(X, dim, func)` is a simple wrapper around `split_along_dim(X,
#' dim) %>% map(func)`. It is conceptually and functionally equivalent to
#' `base::apply()`, with the following key differences:
#'
#' + it is guaranteed to return a list (`base::apply()` attempts to simplify the
#' output to an array, sometimes unsuccessfully, making the output unstable)
#'
#' + it accepts the compact lambda notation `~.x` just like in [`purrr::map`]
#' (and [`modify_along_dim()`])
#'
#'
#' @param X an R array
#' @param .dim which dimension to map along. Passed on to [`split_along_dim()`],
#'   and accepts all the same inputs. Valid inputs include
#'
#'  *   positive integers (index position(s) of dimension),
#'  *   negative integers (index positions(s) of dimensions, counting from the back), or
#'  *   character vector (corresponding to array dimnames)
#' @param .f A function, string of a function name, or `purrr` style compact lambda syntax (e.g, `~.x + 1`)
#' @param ... passed on to `.f()`
#' @param .drop passed on to `[` when subsetting the array.
#'
#' @return An R list
#' @export
#'
#' @rdname map_along_dim
#' @examples
#' X <- matrix2(letters[1:15], ncol = 3)
#'
#' apply(X, 1, function(x) paste(x, collapse = ""))   # simplifies to a vector
#' map_along_dim(X, 1, ~paste(.x, collapse = ""))     # returns a list
#'
#' identical(
#'   map_along_rows(X, identity),
#'   map_along_dim(X, 1, identity)) # TRUE
#'
#' identical(
#'   map_along_cols(X, identity),
#'   map_along_dim(X, -1, identity)) # TRUE
map_along_dim <- function(X, .dim, .f, ..., .drop = NULL) {
  stopifnot(is.array(X))
  if (requireNamespace("rlang", quietly = TRUE)) {
    .f <- rlang::as_function(.f)
  } else {
    if (inherits(.f, "formula"))
      stop("Specifing functions via forumla syntax requires ",
           "package rlang to be available")
    .f <- match.fun(.f)
  }
  lapply( split_along_dim(X, .dim, drop = .drop), .f, ...)
}

#' @export
#' @rdname map_along_dim
map_along_rows <- function(X, .f, ..., .drop = NULL)
  map_along_dim(X, 1L, .f, ..., .drop = .drop)

#' @export
#' @rdname map_along_dim
map_along_cols <- function(X, .f, ..., .drop = NULL)
  map_along_dim(X, -1L, .f, ..., .drop = .drop)



## Maybe add this?
# map_*_dim <- function(x, which_dim, .f) {
#   map(x, .f) %>%
#     bind_*_dim()
# }
