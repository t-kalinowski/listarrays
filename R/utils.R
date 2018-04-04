

#' @export
ndim <- function(x) {
  if (is.null(dx <- dim(x)))
    1L
  else
    length(dx)
}

`%||%` <- function (x, y) {
  if (is.null(x))
    y
  else
    x
}

#' @importFrom compiler cmpfun
parse1 <- function(text) parse(text = text, keep.source = FALSE)[[1]]


is.negative <- function(x) x < 0

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

p0 <- function(...) paste0(...)

dropNULLs <- function(x) x[!vapply(x, is.null, TRUE)]



quick_cbind <- function(lst) {
  x <- unlist(lst)
  dim(x) <- c(length(lst[[1]]), length(lst))
  x
}
