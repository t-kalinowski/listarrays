
ndims <- function(x) length(dim(x) %||% 1L)

`%||%` <- function (x, y) {
  if (is.null(x))
    y
  else
    x
}

parse1 <- function(chr) parse(text = chr, keep.source = FALSE)[[1]]

#' @importFrom compiler cmpfun
# parse_and_compile <- function(chr, env = parent.frame())
#   compile(parse(text = chr, keep.source = FALSE), env = env)
#
# compile <- compiler::compile

# eval_text <- function(text, env = parent.frame())
#   eval(compile(parse(text = text, keep.source = FALSE), env), env)
# b <- browser
# eval_text <- function(text, compile = TRUE, env = parent.frame()) {
#   x <- parse(text = text, keep.source = FALSE)
#   # b()
#   if (compile)
#     x <- compile(x, env)
#   eval(x, env)
# }


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


p0 <- function(...) paste0(...)

dropNULLs <- function(x) x[!vapply(x, is.null, logical(1))]

