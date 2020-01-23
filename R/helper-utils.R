
arr <- function(..., mode = "integer") {
  array(as.vector(seq_len(prod(...)), mode = mode), dim = c(...))
}
