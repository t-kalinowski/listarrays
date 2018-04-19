
#' Reshape an array to bring a dim forward
#'
#' @param X an array
#' @param which_dim scalar integer or string, which dim to bring forward. Negative numbers count from the back
#'
#' This is a powered by `base::aperm()`.
#'
#' @return a reshaped array
#' @export
#'
#' @seealso `base::aperm()` `set_dim()` `keras::array_reshape()`
#'
#' @examples
#' x <- array(1:24, 2:4)
#' y <- set_as_rows(x, 3)
#'
#' for (i in seq_along_dim(x, 3))
#'   stopifnot( identical(x[,,i], y[i,,]) )
set_as_rows <- function(X, which_dim) {
  stopifnot(is.array(X))

  which_dim <- standardize_which_dim(which_dim, X)

  cur_dim_order <- seq_along(dim(X))
  new_dim_order <- c(which_dim, cur_dim_order[-which_dim])

  aperm(X, new_dim_order)
}

# other name ideas:
# bring_dim_forward()

#' @export
#' @rdname set_as_rows
set_as_cols <- function(X, which_dim) {
  stopifnot(is.array(X))
  which_dim <- standardize_which_dim(which_dim, X)

  cur_dim_order <- seq_along(dim(X))
  new_dim_order <- c(cur_dim_order[-which_dim], which_dim)

  aperm(X, new_dim_order)
}
