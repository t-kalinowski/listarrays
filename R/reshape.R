
#' Reshape an array to bring a dim forward
#'
#' @param X an array
#' @param .dim scalar integer, which dim to bring forward
#'
#' @return a reshaped array
#' @export
#'
#' @examples
#' x <- array(1:24, 2:4)
#' y <- set_as_rows(x, 3)
#'
#' for (i in seq_along_dim(x, 3))
#'   stopifnot( identical(x[,,i], y[i,,]) )
set_as_rows <- function(X, .dim) {
  stopifnot(is.array(X),
            identical(length(.dim), 1L))

  if(is.character(.dim)) {
    dnn <- names(dimnames(.dim))

    if(is.null(dnn) || any(dnn == ""))
      stop("X must have axis names set if .dim is a character")

    if(.dim %not_in% dnn)
      stop(".dim must match to an axis name of .dim")

    new_d <- unique(c(.dim, dnn))

  } else {

    check.is.integerish(.dim, n = 1L)
    cur_d <- seq_along(dim(X))
    new_d <- c(.dim, cur_d[-.dim])

  }

  aperm(X, new_d)
}

# other name ideas:
# bring_dim_forward()
