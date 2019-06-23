




#' Length of `DIM()`
#'
#' Returns the number of dimensions, or 1 for an atomic vector.
#' @param x a matrix or atomic vector
#' @export
ndim <- function(x) {
  if (is.null(dx <- dim(x)))
    1L
  else
    length(dx)
}



#' Helpers for working with 1-d arrays
#'
#' `DIM()` is to `dim()` as `NROW()` is to `nrow()`. That is, it is identical to
#' `dim()` in most cases except if the input is a bare atomic vector with no
#' `dim` attribute, in which case, the length of the vector is returned instead
#' of `NULL`.
#'
#' @param x an array or atomic vector
#' @rdname DIM
#'
#' @export
#' @return For `DIM`, the `dim` attribute, or if that's not found, then `length(x)`
#' @examples
#' x <- 1:3
#' dim(x)
#' dim(array(x))
#'
#' DIM(x)
#' DIM(array(x))
#'
DIM <- function(x) dim(x) %||% length(x)


#' DROP
#'
#' `DROP` first calls `base::drop`, but and then completely removes the `dim`
#' attribute if the result is a 1-d array
#'
#' @param an R vector, potentially with dim attributes
#'
#' @return For `DROP` an array with 2 or more axes, or a vector with no `dim`
#'   attributes.
#' @export
#' @rdname DIM
#'
#' @examples
#' x <- array(1:3)
#' str(drop(x))
#' str(DROP(x))
DROP <- function(x) {
  x <- drop(x)
  if(identical(length(DIM(x)), 1L))
    dim(x) <- NULL
  x
}


`%||%` <- function (x, y) {
  if (is.null(x))
    y
  else
    x
}

#' @importFrom compiler cmpfun

parse1 <- function(x) parse(text = x, keep.source = FALSE)[[1]]

is.negative <- function(x) x < 0

is.integerish <- function(x, n = NULL, allow_na = FALSE) {
  if (!is.null(n) && n != length(x))
    return(FALSE)
  if (!allow_na && any(is.na(x) | is.infinite(x)))
    return(FALSE)
  if (identical(typeof_x <- typeof(x), "integer"))
    return(TRUE)
  if (identical(typeof_x, "double"))
    return(all(x == as.integer(x), na.rm = TRUE))
  FALSE
}


is.scalar <- function(x) identical(length(x), 1L)

is.scalar.integerish <- function(x)
  is.scalar(x) && is.integerish(x)


`%not_in%` <- function(x, y) match(x, y, nomatch = 0L) == 0L

check.is.integerish <- function(x, n = NULL) {
  nm <- deparse(substitute(x))
  if(!(is.integerish(x, n))) {
    msg <- paste(nm, "must be an integer")
    if (!is.null(n))
      msg <- paste(msg, "of length", n)
    stop(msg, call. = FALSE)
  }
}

dropNULLs <- function(x) x[!vapply(x, is.null, TRUE)]


quick_cbind <- function(lst) {
  x <- unlist(lst)
  dim(x) <- c(length(lst[[1]]), length(lst))
  x
}

# arr <- function(...) array(seq_len(prod(unlist(c(...)))), unlist(c(...)))
