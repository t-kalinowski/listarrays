



#' Convert vector to a onehot representation (binary class matrix)
#'
#' @param y character, factor, or numeric vector
#' @param Y a matrix, as returned by `onehot()` or similar.
#' @param order `NULL`, `FALSE`, or a character vector. If `NULL` (the default),
#'   then levels are sorted with `sort()`. If `FALSE`, then levels are taken in
#'   order of their first appearance in `y`. If a character vector, then `order`
#'   must contain all levels found in `y`.
#' @param named if the returned matrix should have column names
#' @param classes A character vector of class names in the order corresponding
#'   to `Y`'s onehot encoding. Typically, `colnames(Y)`. if `NULL`, then the
#'   decoder returns the column number.
#' @param n_classes The total number of classes expected in `Y`. Used for input
#'   checking in the returned decoder, also, to reconstruct the correct
#'   dimensions if the passed in `Y` is missing `dim()` attributes.
#'
#' @return A binary class matrix
#' @export
#' @seealso [keras::to_categorical]
#' @rdname onehot
#'
#' @examples
#' if(require(zeallot)) {
#'   y <- letters[1:4]
#'   c(Y, decode) %<-% onehot_with_decoder(y)
#'   Y
#'   decode(Y)
#'   identical(y, decode(Y))
#'   decode(Y[2,,drop = TRUE])
#'   decode(Y[2,,drop = FALSE])
#'   decode(Y[2:3,])
#'
#'   rm(Y, decode)
#' }
#'
#' # more peicemeal functions
#' Y <- onehot(y)
#' decode_onehot(Y)
#'
#' # if you need to decode a matrix that lost colnames,
#' # make your own decoder that remembers classes
#' my_decode <- onehot_decoder(Y)
#' colnames(Y) <- NULL
#' my_decode(Y)
#' decode_onehot(Y)
#'
#' # factor and numeric vectors also accepted
#' onehot(factor(letters[1:4]))
#' onehot(1:4)
#'
#' # for numerics, all numbers between 1:max(y) are assumed to
#' # be part of the set
#' onehot(4:8)
#'
#' # pass as a character vector to override, also sorted class names
#' onehot(paste(4:8), paste(4:8))
onehot_with_decoder <- function(y, order = NULL, named = TRUE) {

  Y <- onehot(y, order = order, named = TRUE)
  decode <- onehot_decoder(classes = colnames(Y),
                               n_classes = ncol(Y))

  if(!isTRUE(named))
    colnames(Y) <- NULL

  list(onehot = Y, decode = decode)
}


#' @export
#' @rdname onehot
onehot <- function(y, order = NULL, named = TRUE) {

  if (is.factor(y)) {

    if (!missing(order))
      warning("value supplied to `order` is ignored since y is a factor")

    order <- levels(y)
    ncols <- length(order)
    idx_col <- unclass(y)

  } else if (is.character(y) || is.numeric(y)) {

    if (is.null(order))
      order <- sort(unique(y))
    else if (identical(order, FALSE))
      order <- unique(y)
    else if (is.character(order) || is.numeric(order))
      stopifnot(typeof(y) == typeof(order), unique(y) %in% order)
    else
      stop("`order` must be NULL, FALSE, or a character vector")

    idx_col <- match(y, order)
    ncols <- length(order)

  } else
    stop("`y` must be a character, numeric, or factor")

  Y <- matrix(0, ncol = ncols, nrow = length(y))
  idx <- cbind(seq_along(y), idx_col, deparse.level = 0L)
  Y[idx] <- 1

  if(named)
    colnames(Y) <- as.character(order)

  Y
}


#' @export
#' @rdname onehot
decode_onehot <- function(Y, classes = colnames(Y),
                          n_classes =  ncol(Y) %||% length(classes)) {
  decode <- onehot_decoder(classes = classes, n_classes = n_classes)
  decode(Y)
}

#' @export
#' @rdname onehot
onehot_decoder <- function(Y, classes = colnames(Y), n_classes = length(classes)) {
  force(classes)
  n_classes <- as.integer(n_classes)
  rm(Y)

  if(n_classes <= 0L || is.na(n_classes))
    stop("`n_classes` must be a scalar integer greater than 0")

  robust_max.col <- function(m) {
    if (is.matrix(m))
      stopifnot(identical(ncol(m), n_classes))
    else { # dim was probably dropped by [, drop = TRUE]
      if (length(m) %% n_classes)
        stop("length(Y) must be a multiple of n_classes, ", n_classes,
             ", not", length(m))
      dim(m) <- c(length(m) %/% n_classes, n_classes)
    }

    max.col(m)
  }

  if (is.null(classes)) {
    function(Y) robust_max.col(Y)
  } else {
    function(Y) classes[robust_max.col(Y)]
  }
}
