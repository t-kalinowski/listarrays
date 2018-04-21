
#' Make or reshape an array with C-style (row-major) semantics
#'
#' These functions reshape or make an array using C-style, row-major semantics.
#' The returned array is still R's native F-style, (meaning, the underlying
#' vector has been reordered).
#'
#' Other than the C-style semantics, these functions behave identically to their
#' counterparts (`array2()` behaves identically to `array()`, \code{`dim2<-`()}
#' to \code{`dim<-`()}). `set_dim2()` is just a wrapper around `set_dim(...,
#' order = "C")`.
#'
#' See examples for a drop-in pure R replacement to `reticulate::array_reshape()`
#'
#' @param data what to fill the array with
#' @param dim numeric vector of dimensions
#' @param dimnames a list of dimnames, must be the same length as `dims`
#'
#' @export
#' @examples
#' array(1:4, c(2,2))
#' array2(1:4, c(2,2))
#'
#' # for a drop-in replacement to reticulate::array_reshape
#' array_reshape <- listarrays:::array_reshape
#' array_reshape(1:4, c(2,2))
array2 <- function(data, dim = length(data), dimnames = NULL) {
  pd <- prod(dim)
  if(length(data) != pd)
    data <- rep_len(data, pd)
  dim2(data) <- dim
  dimnames(data) <- dimnames
  data
}


#' @export
#' @rdname array2
#' @param x object to set dimensions on (array or atomic vector)
#' @param value a numeric (integerish) vector of new dimensions
`dim2<-` <- function(x, value) {
  if(is.null(value)) {
    x <- t(x)
    dim(x) <- NULL
    return(x)
  }

  dim_x <- dim(x)
  if(identical(dim_x, as.integer(value)))
    return(x)

  if (!is.null(dim_x))
    x <- t(x)

  dim(x) <- rev(value)
  t(x)
}


#' @export
#' @rdname array2
#' @param ... passed on to `set_dim()`
set_dim2 <- function(...) {
  set_dim(..., order = "C")
}



# equivelant to reticulate::array_reshape(),
# but a pure R solution (and therefore usually faster)
array_reshape <- function(x, dim, order = c("C", "F")) {

  # rename to avoid possible recursive loop when calling dim()
  # arg is named `dim` for compatability with reticulate::array_reshape()
  new_dim <- dim; rm(dim)

  order <- match.arg(order)
  if (identical(order, "C"))
    dim2(x) <- new_dim
  else
    dim(x) <- new_dim

  x
}


#' transpose an array
#'
#' @param x an array
#'
#' This reverses the dimensions of an array
#'
#' @export
#' @examples
#' x <- array(1:27, c(3,3,3))
#' tx <- t(x)
#' for (i in 1:3)
#'   for(j in 1:3)
#'     stopifnot(x[,j,i] == tx[i,j,])
t.array <- function(x) aperm(x, length(dim(x)):1)
