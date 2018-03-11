
ndims <- function(x) length(dim(x) %||% 1L)


`%||%` <- function (x, y) {
  if (is.null(x))
    y
  else
    x
}

just_parse <- function(chr)
  parse(text = chr, keep.source = FALSE)[[1]]

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