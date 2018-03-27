

#' Bind arrays along a specified dimension
#'
#' `bind_as_*` introduces a new dimension, such that each element in
#' `list_of_arrays` corresponds to one index position along the new dimension in
#' the returned array. `bind_on_*` binds all elements along an existing
#' dimension, (meaning, the returned array has the same number of dimensions as
#' each of the arrays in the list).
#'
#' `bind_*_rows()` is a wrapper for the common case of `bind_*_dim(X, 1)`.
#'
#' @param list_of_arrays a list of arrays. All arrays must be of the same
#'   dimension. NULL's in place of arrays are automatically dropped.
#' @param ... Arrays to be bound, specified individually or supplied as a single list
#' @param .dim Scalar integer, specifying the index position of where to
#'   introduce the new dimension to introduce.
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
bind_as_dim <- function(list_of_arrays, .dim) {
  check.is.integerish(.dim, 1L)

  # TODO, .dim should accept a named vector, in which case it sets a new dimname
  # e.g., .dim = c(channels = 3)
  # if(!is.null(names(.dim)))
  #   new_dimname <- names(.dim)

  # TODO, think if `list_of_arrays` should instead be ..., downside there is
  # .dim then must be named

  stopifnot(is.list(list_of_arrays))
  list_of_arrays <- dropNULLs(list_of_arrays)

  base_dim <- unique(lapply(list_of_arrays, function(x) dim(x) %||% length(x)))

  stopifnot(length(base_dim) == 1)
  base_dim <- base_dim[[1]]

  new_dim <- append(base_dim, length(list_of_arrays), after = .dim - 1L)

  X <- array(vector(typeof(list_of_arrays[[1]])), dim = new_dim)

  Xi <- extract_dim_chr_expr(X, .dim, .idx_var = "i")
  expr <- parse1(paste0(Xi, " <- list_of_arrays[[i]]"))

  e <- environment()
  eval <- maybe_eval_bare()

  for(i in seq_along(list_of_arrays))
    eval(expr, e)

  if(!is.null(names(list_of_arrays)))
    dimnames(X)[[.dim]] <- names(list_of_arrays)

  X
}

#' @rdname bind-arrays
#' @export
bind_as_rows <- function(...) {
  list_of_arrays <- list(...)
  if (identical(nargs(), 1L))
    list_of_arrays <- list_of_arrays[[1]]
  bind_as_dim(list_of_arrays, .dim = 1L)
}



#' @rdname bind-arrays
#' @export
bind_on_dim <- function(list_of_arrays, .dim) {

  stopifnot(is.list(list_of_arrays))
  list_of_arrays <- dropNULLs(list_of_arrays)

  if(is.character(.dim)) {
    all_axis_names <- lapply(list_of_arrays, function(x) names(dimnames(x)))
    stopifnot(length(unique(all_axis_names)) == 1L)
    axis_names <- all_axis_names[[1]]
    .dim <- match(.dim, axis_names)
  }
  check.is.integerish(.dim, 1L)


  all_dims <- lapply(list_of_arrays, function(x) dim(x) %||% length(x))

  base_dim <- unique(lapply(all_dims, function(d) d[-.dim]))
  stopifnot(length(base_dim) == 1L)
  base_dim <- base_dim[[1]]

  n_entries_per_array <- vapply(all_dims, function(d) d[.dim], 1L)

  new_dim <- all_dims[[1]]
  new_dim[.dim] <- sum(n_entries_per_array)

  X <- array(vector(typeof(list_of_arrays[[1]])), dim = new_dim)

  Xi <- extract_dim_chr_expr(X, .dim, .idx_var = "start:end")
  expr <- parse1(paste0(Xi, " <- list_of_arrays[[i]]"))

  eval <- maybe_eval_bare()
  e <- environment()

  start <- 1L
  for(i in seq_along(list_of_arrays)) {
    end <- start + n_entries_per_array[i] - 1L
    eval(expr, e)
    start <- end + 1L
  }

  if(!is.null(names(list_of_arrays)))
    dimnames(X)[[.dim]] <- rep(names(list_of_arrays),
                               times = n_entries_per_array)

  X
}

#' @rdname bind-arrays
#' @export
bind_on_rows <- function(...) {
  list_of_arrays <- list(...)
  if (identical(nargs(), 1L))
    list_of_arrays <- list_of_arrays[[1]]

  bind_on_dim(list_of_arrays, .dim = 1L)
}





## Maybe add this?
# map_*_dim <- function(x, .dim, .f) {
#   map(x, .f) %>%
#     bind_*_dim()
# }

