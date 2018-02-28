
#' @export
set_dimnames <- function(x, nm, .dim = NULL) {
  if(is.null(.dim))
    dimnames(x) <- nm
  else
    for(n in nm)
      dimnames(x)[[.dim]] <- n

  x
}


#' @export
get_dimnames <- function(x, .dim = NULL) {
  if(is.null(.dim))
    dimnames(x)
  else if (length(.dim)  > 1L)
    dimnames(x)[.dim]
  else
    dimnames(x)[[.dim]]
}


#' @export
set_dim <- function(x, .dim) {
  dim(x) <- .dim
  x
}