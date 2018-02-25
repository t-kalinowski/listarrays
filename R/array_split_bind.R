
# map_as_rows
# map_ar


# wrappers around
# for(i in seq_along_dim())
#  x[,i,] <- .f(x[,i,])
#
# modify_along_rows
# modify_along_cols
# modify_along_dims


# wrappers around
#  lapply(seq_along_dim(), function(x) x[,i,])
# split_along_rows
# split_along_cols
# split_along_dims


# bind along a new dimension
# bind_as_rows
# bind_as_cols
# bind_as_dims

# # bind along an existing dimension
# bind_along_rows
# bind_along_cols
# bind_along_dim

# '  @S3method modify array




#' Modify an array by mapping over 1 or more dimensions
#'
#' @param X An array, or a list of arrays
#' @param .dim integer vector of dimensions to modify at
#' @param .f a function or formula defining a function(same semantics as
#'   [`purrr::map`]). The function must return either an array the same shape as
#'   it was passed, a vector of the same length, or a scalar, although the type
#'   of the returned object does not need to be the same as was passed in.
#' @param ... passed on to `.f()`
#'
#' @export
#' @rdname modify_along_dim
#' @return An array, or if `X` was a list, a list of arrays of the same shape as
#'   was passed in.
#' @examples
#' x <- array(1:6, 1:3)
#' modify_along_dim(x, 3, ~mean(.x))
#' modify_along_dim(x, 3, ~.x/mean(.x))
modify_along_dim <- function(X, .dim, .f, ...) {

  if(is.list(X))
    return(lapply(X, function(x) modify_along_dim(x, .dim, .f, ...)))


  if(inherits(.f, "formula")) {
    if( !requireNamespace("rlang") )
      stop("Specifing functions via forumla syntax requires package rlang to be available")

    .f <- rlang::as_function(.f)
  } else
    .f <- match.fun(.f)



  names(.dim) <- paste0("idx", seq_along(.dim))

  Xe <- extract_dim_chr_expr( X, .dim,
    .idx_var = paste0("combs$", names(.dim), "[[r]]"),
    .var_to_subset = "X")

  oXe <- paste0("o", Xe)
  oX <- X

  expr <- parse(text = paste0(Xe, " <- .f(", oXe, ", ...)"))[[1]]

  combs <- as.list(do.call(
    function(...) expand.grid(..., KEEP.OUT.ATTRS = FALSE),
    lapply(.dim, seq_len)
  ))

  for (r in seq_along_rows(combs))
    eval(expr)

  X
}


#' @export
#' @rdname modify_along_dim
modify_along_rows <- function(X, .f, ...)
  modify_along_dim(X, 1L, .f, ...)

#' @export
#' @rdname modify_along_dim
modify_along_cols <- function(X, .f, ...)
  modify_along_dim(X, 2L, .f, ...)


#' Split an array along a dimension
#'
#' `split_along_dim(X, .dim)` is equivalent to
#' `split_on_dim(X, seq_along_dim(X, .dim))`
#'
#' @param X an array, or list of arrays. Atomic vectors without a dimension
#'   attribute is treated as a 1 dimensions array. Names of list are preserved.
#' @param .dim a scalar integer, specifying which dimension to split along
#' @param f a vector or list of vectors. Must be the same length as the dimension being split. Passed on to `base::split()` (also, `base::interaction()` if a list).
#' @param drop passed on to `[`.
#' @param .keep_names Logical. If `TRUE` then if the dim being split along has
#'   dimnames, then the returned list has those names.
#'
#' @return A list of arrays, or if a list of arrays was passed in, then a list
#'   of lists of arrays.
#' @rdname split-array
#' @export
#'
#' @examples
#' X <- array(1:8, c(2,3,4))
#' X
#' split_along_dim(X, 2)
#' split_on_dim(X, 2, c("a", "a", "b"), drop = FALSE)
split_on_dim <- function(X, .dim,
                         f = dimnames(X)[[.dim]] %||% seq_along_dim(X, .dim),
                         drop = NULL) {

  if(is.list(f))
    f <- interaction(f, drop = TRUE)


  if (is.list(X))
    lapply(X, function(x) split_on_dim(x, .dim, f = f, drop = drop))
  else {
    id <- seq_along_dim(X, .dim)
    if(!identical(length(id), length(f)))
      stop("`f` must be the same length as the dimension being split on.")
    l <- split(id, f)
    lapply(l, function(idx) extract_dim(X, .dim, idx, drop = drop))
  }
}

#' @rdname split-array
#' @export
split_on_rows <- function(X,
                          f = rownames(X) %||% seq_along_rows(X),
                          drop = NULL)
  split_on_dim(X, 1L, f = f, drop = drop)


#' @rdname split-array
#' @export
split_on_cols <- function(X,
                          f = colnames(X) %||% seq_along_cols(X),
                          drop = NULL)
  split_on_dim(X, 2L, f = f, drop = drop)


#' @rdname split-array
#' @export
split_along_dim <- function(X, .dim, drop = NULL, .keep_names = TRUE) {
  if (is.list(X))
    return(lapply(X, function(x)
      split_along_dim(x, .dim, drop = drop, .keep_names = .keep_names)))


  out <- lapply(seq_along_dim(X, .dim),
                function(i)
                  extract_dim(X, .dim, i, drop = drop))

  if (isTRUE(.keep_names) && !is.null(nms <- dimnames(X)[[.dim]]))
    names(out) <- nms

  out
}

#' @rdname split-array
#' @export
split_along_rows <-
  function(X, drop = NULL, .keep_names = TRUE)
    split_along_dim(X, 1L, drop = drop, .keep_names = .keep_names)

#' @rdname split-array
#' @export
split_along_cols <-
  function(X, drop = NULL, .keep_names = TRUE)
    split_along_dim(X, 2L, drop = drop, .keep_names = .keep_names)



#' Bind arrays along a specified dimension
#'
#' `bind_as_*` introduces a new dimension, such that each element in
#' `list_of_arrays` corresponds to one index position along the new dimension in
#' the returned array. `bind_on_*` binds all elements along an existing
#' dimension.
#'
#' `bind_*_rows()` is a wrapper for the common case of `bind_*_dim(X, 1)`.
#'
#' `bind_*_cols()` is a wrapper for the common case of `bind_*_dim(X, 2)`.
#'
#' @param list_of_arrays a list of arrays. All arrays must be of the same
#'   dimension.
#' @param .dim Scalar integer, specifying the index position of where to
#'   introduce the new dimension to introduce.
#' @param .keep_names Whether `names(list_of_arrays)` should be used to
#'   construct dimnames.
#'
#' @return An array, with one additional dimension.
#' @rdname bind-arrays
#' @export
#'
#' @examples
#' list_of_arrays <- replicate(10, array(1:8, dim = c(2,3,4)), FALSE)
#'
#' dim(list_of_arrays[[1]])
#'
#' # bind on a new dimension
#' combined_as <- bind_as_rows(list_of_arrays)
#' dim(combined_as)
#' dim(combined_as)[1] == length(list_of_arrays)
#'
#' # each element in `list_of_arrays` corresponds to one "row"
#' # (i.e., one entry in along the first dimension)
#' for(i in seq_along(list_of_arrays))
#'   stopifnot(identical(combined_as[i,,,], list_of_arrays[[i]]))
#'
#' # bind on an existing dimension
#' combined_on <- bind_on_rows(list_of_arrays)
#' dim(combined_on)
#' dim(combined_on)[1] == sum(sapply(list_of_arrays, function(x) dim(x)[1]))
#' identical(list_of_arrays[[1]], combined_on[1:2,,])
#' for (i in seq_along(list_of_arrays))
#'   stopifnot(identical(
#'     list_of_arrays[[i]], combined_on[ (1:2) + (i-1)*2,,]
#'   ))
#'
#' # bind on any dimension
#' combined <- bind_as_dim(list_of_arrays, 3)
#' dim(combined)
#' for(i in seq_along(list_of_arrays))
#'    stopifnot(identical(combined[,,i,], list_of_arrays[[i]]))
bind_as_dim <- function(list_of_arrays, .dim, .keep_names = TRUE) {
  check_is.integerish(.dim, 1L)

  stopifnot(is.list(list_of_arrays))

  for (i in seq_along(list_of_arrays))
    list_of_arrays[[i]] <- as.array(list_of_arrays[[i]])

  base_dim <- unique(lapply(list_of_arrays, dim))

  stopifnot(length(base_dim) == 1)
  base_dim <- base_dim[[1]]

  new_dim <- append(base_dim, length(list_of_arrays), after = .dim - 1L)

  X <- array(vector(typeof(list_of_arrays[[1]])), dim = new_dim)

  Xi <- extract_dim_chr_expr(X, .dim, .idx_var = "i")
  expr <- parse(text = p0(Xi, " <- list_of_arrays[[i]]"))
  for(i in seq_along(list_of_arrays))
    eval(expr)

  if(.keep_names && !is.null(names(list_of_arrays)))
    dimnames(X)[[.dim]] <- names(list_of_arrays)

  X
}

#' @rdname bind-arrays
#' @export
bind_as_rows <- function(list_of_arrays, .keep_names = TRUE)
  bind_as_dim(list_of_arrays, .dim = 1L, .keep_names = .keep_names)

#' @rdname bind-arrays
#' @export
bind_as_cols <- function(list_of_arrays, .keep_names = TRUE)
  bind_as_dim(list_of_arrays, .dim = 2L, .keep_names = .keep_names)




#' @rdname bind-arrays
#' @export
bind_on_dim <- function(list_of_arrays, .dim, .keep_names = TRUE) {

  check_is.integerish(.dim, 1L)
  stopifnot(is.list(list_of_arrays))

  all_dims <- lapply(list_of_arrays, robust_dim)

  base_dim <- unique(lapply(all_dims, function(d) d[-.dim]))
  stopifnot(length(base_dim) == 1)
  base_dim <- base_dim[[1]]

  n_entries_per_array <- vapply(all_dims, function(d) d[.dim], 1L)

  new_dim <- all_dims[[1]]
  new_dim[.dim] <- sum(n_entries_per_array)

  X <- array(vector(typeof(list_of_arrays[[1]])), dim = new_dim)

  Xi <- extract_dim_chr_expr(X, .dim, .idx_var = "start:end")
  expr <- parse(text = p0(Xi, " <- list_of_arrays[[i]]"))

  start <- 1L
  for(i in seq_along(list_of_arrays)) {
    end <- start + n_entries_per_array[i] - 1L
    eval(expr)
    start <- end + 1L
  }

  if(.keep_names && !is.null(names(list_of_arrays)))
    dimnames(X)[[.dim]] <- rep(names(list_of_arrays), times =  n_entries_per_array)

  X
}

#' @rdname bind-arrays
#' @export
bind_on_rows <- function(list_of_arrays, .keep_names = TRUE)
  bind_on_dim(list_of_arrays, .dim = 1L, .keep_names = .keep_names)

#' @rdname bind-arrays
#' @export
bind_on_cols <- function(list_of_arrays, .keep_names = TRUE)
  bind_on_dim(list_of_arrays, .dim = 1L, .keep_names = .keep_names)

