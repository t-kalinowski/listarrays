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
