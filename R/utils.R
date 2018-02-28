





# avoiding automatically calling as.array, since this is going to be used on
# dataframes and thats expensive.
robust_nrow <- function(x) {
  if(is.null(d <- nrow(x)))
    length(x)
  else
    d
}


robust_dim <- function(x) {
  if(is.null(d <- dim(x)))
    length(x)
  else
    d
}



robust_ncol <- function(x) {
  # not sure what the behavior should be for vectors or 1 dimensional arrays
  if (is.null(d <- ncol(x))) {
    if (length(x))
      1L
    else
      0L
  } else
    d
}


ndims <- function(x) length(dim(x) %||% 1L)


get_dim <- function(x, n){
  dx <- dim(x)[[n]]
  dx
}



`%||%` <- function (x, y) {
  if (is.null(x))
    y
  else
    x
}




is.integerish <- function(x, n = NULL) {
  if (!is.null(n) && n != length(x))
    return(FALSE)
  if (any(is.na(x) | is.infinite(x)))
    return(FALSE)
  if (identical(typeof(x), "integer"))
    return(TRUE)
  if (identical(typeof(x), "double"))
    return(all(x == as.integer(x)))
  FALSE
}


is.scalar.integerish <- function(x)
  is.integerish(x, n = 1L)


check.is.integerish <- function(x, n) {
  nm <- deparse(substitute(x))
  if(!(is.integerish(x, n))) {
    msg <- paste(nm, "must be an integer")
    if (!is.null(n))
      msg <- paste(msg, "of length", n)
    stop(msg, call. = FALSE)
  }
}


p0 <- function(...) paste0(...)