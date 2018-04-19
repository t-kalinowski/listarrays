#' Reshape an array
#'
#' Pipe friendly `dim<-()`, with option to pad to necessary length. Also allows
#' for filling the array using C style row-major semantics.
#'
#' @param x A vector or array to set dimensions on
#' @param new_dim,value The desired dimensions (an integer(ish) vector)
#' @param pad The value to pad the vector with. `NULL` (the default) performs no
#'   padding.
#' @param order whether to use row-major (C) or column major (F) style
#'   semantics. The default, "F", corresponds to the default behavior of R's
#'   `dim<-()`, while "C" corresponds to the default behavior of
#'   `reticulate::array_reshape()`, numpy, reshaping semantics commonly
#'   encountered in the python world.
#' @param verbose Whether to emit a message if padding. By default, `FALSE`.
#' @param ... passed on to `set_dim()`
#'
#' `dim2<-()` and reshapes with row-major semantics, and is equivelant to
#' `set_dim(order = 'C')` (and `reticulate::array_reshape()`). `set_dim2()` is a
#' wrapper around `dim2<-`, and equivelant to `set_dim(..., order = "C")`.
#'
#' `array2()` is identical to `array()` with the exception that it uses
#' row-major (a.k.a, C style) semantics.
#'
#' @return Object with dimensions set
#' @export
#' @rdname set_dim
#'
#' @seealso `reticulate::array_reshape()`
#'
#' @examples
#' set_dim(1:10, c(2, 5))
#' try( set_dim(1:7, c(2, 5)) ) # error by default, just like `dim<-`()
#'      set_dim(1:7, c(2, 5), pad = 99)
#'      set_dim(1:7, c(2, 5), pad = 99, order = "C") # fills row-wise
#'
#' y <- x <- 1:4
#' # base::dim<- fills the array column wise
#' dim(x) <- c(2, 2)
#' x
#'
#' # dim2 will fill the array row-wise
#' dim2(y) <- c(2, 2)
#' y
#'
#' identical(x, set_dim(1:4, c(2,2)))
#' identical(y, set_dim(1:4, c(2,2), order = "C"))
#'
#' \dontrun{
#' py_reshaped <- reticulate::array_reshape(1:4, c(2,2))
#' storage.mode(py_reshaped) <- "integer" # reticulate coerces to double
#' identical(y, py_reshaped)
#' # if needed, see listarrays:::array_reshape2() for
#' # a drop-in pure R replacement for reticulate::array_reshape()
#' }
set_dim <- function(x, new_dim,
                    pad = getOption("listarrays.autopad_arrays_with", NULL),
                    order = c("F", "C"),
                    verbose = getOption("verbose")) {

  if (!is.null(pad) && !identical(length(x), needed_len <- prod(new_dim))) {
    stopifnot(identical(length(pad), 1L))
    if (verbose)
      message("Padding vector with ", pad, "s",
              " from length ", length(x), " to length ", needed_len)
    x <- c(x, rep_len(pad, needed_len - length(x)))
  }

  order <- match.arg(order)
  if (identical(order, "C"))
    dim2(x) <- new_dim
  else
    dim(x) <- new_dim

  x
}

#' @export
#' @rdname set_dim
set_dim2 <- function(...) {
  set_dim(..., order = "C")
}

#' @export
#' @rdname set_dim
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

#' Fill Array with C-style row-major
#'
#' This fills an array as-if it were a C-style, row-major. The returned array is
#' still R's native F-style. Otherwise, it behaves identically to `base::array()``
#'
#' @param data what to fill the array with
#' @param dims numeric vector of dimensions
#' @param dimnames a list of dimnames, must be the same length as `dims`
#'
#' @export
array2 <- function(data, dims, dimnames) {
  pd <- prod(dims)
  if(length(data) != pd)
    data <- rep_len(data, pd)
  dim2(data) <- dims
  dimnames(data) <- dimnames
  data
}



# equivelant to reticulate::array_reshape(),
# but a pure R solution (and therefore usually faster)
array_reshape <- function(x, dim, order = c("C", "F")) {

  # rename to avoid possible recursive loop when calling dim()
  # arg is named `dim` for compatability with reticulate::array_reshape()
  .dim <- dim; rm(dim)

  order <- match.arg(order)
  if (identical(order, "C"))
    dim2(x) <- .dim
  else
    dim(x) <- .dim

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
