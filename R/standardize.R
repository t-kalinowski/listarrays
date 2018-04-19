
standardize_which_dim <- function(which_dim, X,
                                  n_dim = ndim(X),
                                  names_dimnames_X = names(dimnames(X)),
                                  multiple_OK = FALSE
                                  ) {
  # 3 valid inputs
  # a) string for a name
  # b) negative number for counting backwards
  # c) positive integer (canonical)
  # outputs:
  # case c always

  if (isTRUE(multiple_OK))
    return(vapply(which_dim, function(d)
      standardize_which_dim(d, X, n_dim, names_dimnames_X, multiple_OK = FALSE),
      1L))

  if (is.character(which_dim)) {
    stopifnot(is.scalar(which_dim))
    which_dim <- match(which_dim, names_dimnames_X)
    if(is.na(which_dim))
      stop("'which_dim %in% names(dimnames(X))' must be TRUE")

  } else if (is.scalar.integerish(which_dim)) {
    which_dim <- as.integer(which_dim)

    stopifnot(abs(which_dim) <= n_dim)

    if(which_dim < 0L)
      which_dim <- n_dim + which_dim + 1L

  } else
    stop("`which_dim` must be a positive or negative integer, or character string")

  which_dim

}