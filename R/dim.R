#' Reshape an array
#'
#' Pipe friendly `dim<-()`, with option to pad to necessary length. Also allows
#' for filling the array using C style row-major semantics.
#'
#' @param x A vector or array to set dimensions on
#' @param new_dim The desired dimensions (an integer(ish) vector)
#' @param pad The value to pad the vector with. `NULL` (the default) performs no
#'   padding.
#' @param order whether to use row-major (C) or column major (F) style
#'   semantics. The default, "F", corresponds to the default behavior of R's
#'   `dim<-()`, while "C" corresponds to the default behavior of
#'   `reticulate::array_reshape()`, numpy, reshaping semantics commonly
#'   encountered in the python world.
#' @param verbose Whether to emit a message if padding. By default, `FALSE`.
#'
#'
#' @return Object with dimensions set
#' @export
#' @rdname set_dim
#'
#' @seealso  `set_dim2()`, \code{`dim<-`()}, `reticulate::array_reshape()`
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
#' # if needed, see listarrays:::array_reshape() for
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



#' Expand the shape of an array
#'
#' This is analogous to python's `numpy.expand_dims()`, but vectorized on
#' `which_dim`.
#'
#' @param x an array. Bare vectors are treated as 1-d arrays.
#' @param which_dim numeric. Desired index position of the new axis or axes in
#'   the returned array. Negative numbers count from the back. Can be any
#'   length.Throws a warning if any duplicates are provided.
#'
#'
#' @return the array `x` with new dim
#' @export
#'
#' @examples
#' x <- array(1:24, 2:4)
#' dim(x)
#' dim(expand_dims(x))
#' dim(expand_dims(x, 2))
#' dim(expand_dims(x, c(1,2)))
#' dim(expand_dims(x, c(1,-1)))
#' dim(expand_dims(x, 6)) # implicitly also expands dims 4,5
#' dim(expand_dims(x, 4:6))
#'
#' # error, implicit expansion with negative indexes not supported
#' try(expand_dims(x, -6))
#'
#' # supply them explicitly instead
#' dim(expand_dims(x, -(4:6)))
expand_dims <- function(x, which_dim = -1L) {
  d <- DIM(x)
  nd <- length(d)
  nwd <- length(which_dim)

  stopifnot(is.integerish(which_dim))
  wd <- which_dim
  storage.mode(wd) <- "integer"


  neg <- wd < 0L
  if(any(neg))
    wd[neg] <- wd[neg] + nd + nwd + 1L

  if (min(wd) < 1L)
    stop("Implicit additional dims for expansion with negative indexes not supported")

  if ((max_wd <- max(wd)) > nd + nwd) {
    # implicitly pad on right
    wd <- unique(c(wd, (nd + 1L):max_wd))
    ndout <- max_wd
  } else
    ndout <- nd + nwd


  if(anyDuplicated(wd)) {
    warning("Duplicate axis specified, ignored")
    wd <- unique(wd)
  }

  dims <- rep(1L, ndout)
  dims[-wd] <- d

  dim(x) <- dims
  x
}
