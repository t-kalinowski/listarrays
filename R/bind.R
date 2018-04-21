#' Bind arrays along a specified dimension
#'
#' `bind_as_*` introduces a new dimension, such that each element in
#' `list_of_arrays` corresponds to one index position along the new dimension in
#' the returned array. `bind_on_*` binds all elements along an existing
#' dimension, (meaning, the returned array has the same number of dimensions as
#' each of the arrays in the list).
#'
#' `bind_*_rows()` is a wrapper for the common case of `bind_*_dim(X, 1)`.
#' `bind_*_cols()` is a wrapper for the common case of `bind_*_dim(X, -1)`.
#'
#' @param list_of_arrays a list of arrays. All arrays must be of the same
#'   dimension. NULL's in place of arrays are automatically dropped.
#' @param ... Arrays to be bound, specified individually or supplied as a single
#'   list
#' @param which_dim Scalar integer specifying the index position of where to
#'   introduce the new dimension to introduce. Negative numbers count from the
#'   back. For example, given a 3 dimensional array, `-1`, is equivalent to `3`,
#'   `-2` to `2` and `-3` to `1`.
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
bind_as_dim <- function(list_of_arrays, which_dim) {

  # standardize_which_dim(which_dim, n_dim = )
  check.is.integerish(which_dim, 1L)
  new_axis_nm <- names(which_dim)
  which_dim <- as.integer(which_dim)

  stopifnot(is.list(list_of_arrays))
  list_of_arrays <- dropNULLs(list_of_arrays)

  base_dim <- unique(lapply(list_of_arrays, function(x) dim(x) %||% length(x)))

  stopifnot(length(base_dim) == 1)
  base_dim <- base_dim[[1]]

  if(is.negative(which_dim))
    which_dim <- which_dim + length(base_dim) + 2L

  padding_ones <- rep_len(1L, pmax(0, which_dim - length(base_dim) - 1L))
  new_dim <- c(base_dim, padding_ones)
  new_dim <- append(new_dim, length(list_of_arrays), after = which_dim - 1L)

  X <- array(vector(typeof(list_of_arrays[[1]])), dim = new_dim)

  Xi <- extract_dim_chr_expr(var_to_subset = "X", idx_var_nm = "i",
                             which_dim = which_dim, ndims = length(new_dim))

  args <- as.pairlist(alist( list_of_arrays = ))
  body <- parse1("{
    X <- array(vector(typeof(list_of_arrays[[1L]])), dim = new_dim)
    for (i in seq_along(list_of_arrays))
      ",Xi,"  <- list_of_arrays[[i]]
    X
  }")

  bind_it <- eval(call("function", args, body))

  if(length(list_of_arrays) > 100)
    bind_it <- cmpfun(bind_it)

  X <- bind_it(list_of_arrays)

  if(!is.null(names(list_of_arrays)))
    dimnames(X)[[which_dim]] <- names(list_of_arrays)

  if(!is.null(new_axis_nm))
    names(dimnames(X))[which_dim] <- new_axis_nm

  X
}

#' @rdname bind-arrays
#' @export
bind_as_rows <- function(...) {
  list_of_arrays <- list(...)
  if (identical(nargs(), 1L))
    list_of_arrays <- list_of_arrays[[1]]
  bind_as_dim(list_of_arrays, which_dim = 1L)
}

#' @rdname bind-arrays
#' @export
bind_as_cols <- function(...) {
  list_of_arrays <- list(...)
  if (identical(nargs(), 1L))
    list_of_arrays <- list_of_arrays[[1]]
  bind_as_dim(list_of_arrays, which_dim = -1L)
}



#' @rdname bind-arrays
#' @export
bind_on_dim <- function(list_of_arrays, which_dim) {

  stopifnot(is.list(list_of_arrays))
  list_of_arrays <- dropNULLs(list_of_arrays)

  all_dims <- lapply(list_of_arrays, function(x) dim(x) %||% length(x))
  all_n_dims <- lengths(all_dims) #lapply(all_dims, length)
  stopifnot(is.scalar(unique(all_n_dims)))
  n_dim <- all_n_dims[[1]]

  which_dim <- standardize_which_dim(which_dim,
    names_dimnames_X = {
      all_axis_names <- lapply(list_of_arrays, function(x) names(dimnames(x)))
      stopifnot(length(unique(all_axis_names)) == 1L)
      all_axis_names[[1]]
    }, n_dim = n_dim)

  base_dim <- unique(lapply(all_dims, function(d) d[-which_dim]))
  stopifnot(identical(length(base_dim), 1L))
  base_dim <- base_dim[[1]]

  n_entries_per_array <- quick_cbind(all_dims)[which_dim,]

  new_dim <- all_dims[[1]]
  new_dim[which_dim] <- sum(n_entries_per_array)

  X_start_to_end <- extract_dim_chr_expr(
    var_to_subset = "X", idx_var_nm = "start:end",
    which_dim = which_dim, ndims = length(new_dim))

  args <- as.pairlist(alist(
    list_of_arrays =,  n_entries_per_array = ))
  body <- parse1("{
    X <- array(vector(typeof(list_of_arrays[[1L]])), dim = new_dim)
    start <- 1L
    for (i in seq_along(list_of_arrays)) {
      end <- start + n_entries_per_array[[i]] - 1L
      ",X_start_to_end,"  <- list_of_arrays[[i]]
      start <- end + 1L
    }
    X
  }")

  bind_it <- eval(call("function", args, body))

  if(length(list_of_arrays) > 100)
    bind_it <- cmpfun(bind_it)

  X <- bind_it(list_of_arrays, n_entries_per_array)

  if (!is.null(names(list_of_arrays)))
    dimnames(X)[[which_dim]] <-
      rep(names(list_of_arrays), times = n_entries_per_array)

  X
}

#' @rdname bind-arrays
#' @export
bind_on_rows <- function(...) {
  list_of_arrays <- list(...)
  if (identical(length(list_of_arrays), 1L))
    list_of_arrays <- list_of_arrays[[1]]

  bind_on_dim(list_of_arrays, which_dim = 1L)
}

#' @rdname bind-arrays
#' @export
bind_on_cols <- function(...) {
  list_of_arrays <- list(...)
  if (identical(length(list_of_arrays), 1L))
    list_of_arrays <- list_of_arrays[[1]]

  bind_on_dim(list_of_arrays, which_dim = -1L)
}





## Maybe add this?
# map_*_dim <- function(x, which_dim, .f) {
#   map(x, .f) %>%
#     bind_*_dim()
# }
