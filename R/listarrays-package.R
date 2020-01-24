
## usethis namespace: start
#' @useDynLib listarrays, .registration = TRUE
## usethis namespace: end
NULL

#' @export
.split_along_rows <- function(a) {
  .Call("listarrays_split_along_rows", a, PACKAGE = "listarrays")
}
