
## usethis namespace: start
#' @useDynLib listarrays, .registration = TRUE
## usethis namespace: end
NULL

#' @export
.split_along_rows <- function(a, drop = NULL) {
  .Call("listarrays_split_along_rows", a, drop, PACKAGE = "listarrays")
}
