
## usethis namespace: start
#' @useDynLib listarrays, .registration = TRUE
## usethis namespace: end
NULL

#' @export
.split_along_rows <- function(a) {
  .Call("listarrays_split_along_rows", a, PACKAGE = "listarrays")
}

.extract_1row <- function(obj) {
  .Call("extract_1row", obj, PACKAGE = "listarrays")
}
