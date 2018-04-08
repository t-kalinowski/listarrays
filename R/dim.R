
#' Pipe friendly `dim<-()`, with option to pad to necessary length. Also allows
#' for filling the array using C style row-major semantics.
#'
#' `dim2<-()` and reshapes with row-major semantics, and is equivelant to
#' `set_dim(order = 'C')` and `reticulate::array_reshape()`. `set_dim2()` is a
#' wrapper around `dim2<-`, and equivelant to `set_dim(..., order = "C")`.
#'
#' @param x A vector or array to set dimensions on
#' @param .dim,value The desired dimensions (an integer(ish) vector)
#' @param pad The value to pad the vector with. `NULL` (the default) performs no
#'   padding.
#' @param order whether to use row-major (C) or column major (F) style
#'   semantics. The default, "F", corresponds to the default behavior of R's
#'   `dim<-()`, while "C" corresponds to the default behavior of
#'   `reticulate::array_reshape()`, numpy, reshaping semantics commonly
#'   encountered in the python world.
#' @param verbose Whether to emit a message if padding. By default, `FALSE`.
#'
#' @return Object with dimensions set
#' @export
#'
#' @seealso `reticulate::array_reshape()`
#'
#' @examples
#' set_dim(1:10, c(2, 5))
#' try( set_dim(1:7, c(2, 5)) ) # error by default, just like `dim<-`()
#'      set_dim(1:7, c(2, 5), pad = 99)
#'      set_dim(1:7, c(2, 5), pad = 99, order = "C") # fills row-wise
#'
#' y <- x <- 1:4
#' # base::dim<- fills the array column wise
#' dim(x) <- c(2, 2)
#' x
#'
#' # dim2 will fill the array row-wise
#' dim2(y) <- c(2, 2)
#' y
#'
#' identical(x, set_dim(1:4, c(2,2)))
#' identical(y, set_dim(1:4, c(2,2), order = "C"))
#'
#' \dontrun{
#' py_reshaped <- reticulate::array_reshape(1:4, c(2,2))
#' storage.mode(py_reshaped) <- "integer" # reticulate coerces to double
#' identical(y, py_reshaped)
#' # if needed, see listarrays:::array_reshape2() for
#' # a drop-in pure R replacement for reticulate::array_reshape()
#' }
set_dim <- function(x, new_dim,
                    pad = getOption("listarrays.autopad_arrays_with", NULL),
                    order = c("F", "C"),
                    verbose = getOption("verbose")) {

  if (!is.null(pad) && !identical(length(x), needed_len <- prod(new_dim))) {
    stopifnot(identical(length(pad), 1L))
    if (verbose)
      message("Padding vector with ", pad, "s",
              " from length ", length(x), " to length ", needed_len)
    x <- c(x, rep_len(pad, needed_len - length(x)))
  }

  order <- match.arg(order)
  if (identical(order, "C"))
    dim2(x) <- new_dim
  else
    dim(x) <- new_dim

  x
}

# pad_to_length <- function(x, needed_len, pad, verbose) {
#
# }

#' @export
#' @rdname set_dim
set_dim2 <- function(...) {
  set_dim(..., order = "C")
}



#' @export
#' @rdname set_dim
`dim2<-` <- function(x, value) {
  if(is.null(value)) {
    x <- t(x)
    dim(x) <- NULL
    return(x)
  }

  dim_x <- dim(x)
  if(identical(dim_x, as.integer(value)))
    return(x)

  if (!is.null(dim_x))
    x <- t(x)

  dim(x) <- rev(value)
  t(x)
}

# should matrix(..., byrow = TRUE) be used for matrixes?


# transpose
#' @export
t.array <- function(x) aperm(x, length(dim(x)):1)

# other candidate names
# dim_c()
# dim_rows()
# dim_r


# import::from(reticulate, retic_reshape = array_reshape)
# library(magrittr)
# 1:8 %>% array_reshape(c(4, 2))
# 1:8 %>% retic_reshape(c(4, 2))
#
# 1:8 %>% array_reshape(c(4, 2)) %>% array_reshape(c(4, 2))
# 1:8 %>% retic_reshape(c(4, 2)) %>% retic_reshape(c(4, 2))
#
# 1:8 %>% array_reshape(c(4, 2)) %>% array_reshape(c(2, 4))
# 1:8 %>% retic_reshape(c(4, 2)) %>% retic_reshape(c(2, 4))
#
# 1:8 %>% set_dim(c(4, 2)) %>% array_reshape(c(4, 2))
# 1:8 %>% set_dim(c(4, 2)) %>% retic_reshape(c(4, 2))



# equivelant to reticulate::array_reshape(),
# but a pure R solution (and therefore usually faster)
array_reshape <- function(x, dim, order = c("C", "F")) {

  # rename to avoid possible recursive loop when calling dim()
  # arg is named `dim` for compatability with reticulate::array_reshape()
  .dim <- dim; rm(dim)

  order <- match.arg(order)
  if (identical(order, "C"))
    dim2(x) <- .dim
  else
    dim(x) <- .dim

  x
}

#' @export
array2 <- function(data, dims, dimnames) {
  dim2(data) <- dims
  dimnames(data) <- dimnames
  data
}


# is.negative.scalar.integerish <- function(x) is.integerish(x, n = 1L) && x < 0L


# ## better error messages, fold into standardize_which_dim()
# if(is.character(.dim)) {
#   dnn <- names(dimnames(X))
#
#   if(is.null(dnn) || any(dnn == ""))
#     stop("X must have axis names set if .dim is a character")
#
#   if(.dim %not_in% dnn)
#     stop(".dim must match to an axis name of .dim")
#
#   new_d <- unique(c(.dim, dnn))
#
# } else {
#
#   check.is.integerish(.dim, n = 1L)
#   if(.dim > length(dim(X)))
#     stop(".dim must be less than or equal to length(dim(X))")

# TESTING BLOCK
# x <- array(1:8, 2:4)
# standardize_which_dim(x, -1) == 3
# standardize_which_dim(x, -2) == 2
# standardize_which_dim(x, -3) == 1
# standardize_which_dim(x, 3) == 3
# standardize_which_dim(x, 2) == 2
# standardize_which_dim(x, 1) == 1
#
# x <- provideDimnames(x)
# names(dimnames(x)) <- paste0("axis", 1:3)
#
# standardize_which_dim(x, "axis1") == 1
# standardize_which_dim(x, "axis2") == 2
# standardize_which_dim(x, "axis3") == 3
## END TESTING BLOCK
# TODO:
#
# DON 1. make bind_as_dim, seq_along_dim, et.al accept negative integers for
# .dim, equivelant to counting from the back.
#
# DONE 2. bring back *_cols(), this time as seq_along_dim(x, -1L) (ditto for others
# where this makes sense)
#
# 3. factor out the pad_to_length() function
#
# DONE 4. factor out the standardize_which_dim(x, .dim) function
#
# 5. accept a single NA in supplied new_dim in set_dim() (which is then infered
# from the length)
#
# 6. document t.array()
#
# 7. bring in ncol<- ncol2<- and friends
#
# 8. respect options(listarrays.autopad_with)
#
# 9. now with dim2<-(), it seems like listarrays is not the best name. consider
# renaming. candidates: rrays, purrrays, arrays, array
#
# 10. WIP (DONE FOR ALL EXPORTED FUNCTIONS) remove the dot "." prefixes for arguments throughout. .dim -> which_dim or new_dim
#


# import::from(reticulate, array_reshape)
# 1:8 %>% array_reshape(c(4, 2))
# 1:8 %>% array_reshape(c(4, 2)) %>% array_reshape(c(4, 2))
# 1:8 %>% array_reshape(c(4, 2)) %>% array_reshape(c(2, 4))
# 1:8 %>% set_dim(c(4, 2)) %>% array_reshape(c(4, 2))
# 1:8 %>% array_reshape2(c(4, 2))
# 1:8 %>% array_reshape2(c(4, 2)) %>% array_reshape2(c(4, 2))
# 1:8 %>% array_reshape2(c(4, 2)) %>% array_reshape2(c(2, 4))




# #
# library(reticulate)
#
# d <- c(100000, 128, 4)
# ox <- x <- array(1:prod(d), d)
# # dim2(ox) <- c(1000, 512)
#
#
# storage.mode(x) <- "double"
# identical(set_dim(x, c(100000, 512), order = "C"),
#           array_reshape(x, c(100000, 512)))
#
# x <- ox
#           # set_dim(x, c(1000, 512), order = "F"))
#
# r <- microbenchmark::microbenchmark(
#   set_dim(x, c(100000, 512), order = "C"),
#   # set_dim(x, c(1000, 512), order = "F"),
#   array_reshape(x, c(100000, 512)),
#   times = 10
# )
# microbenchmark::autoplot.microbenchmark(r)
# r
#
# reshape2 <- function(x, new_d) {
#   nx <- aperm(x, length(dim(x)):1)
#   dim(nx) <- rev(new_d)
#   aperm(nx, length(new_d):1)
# }
#
# import::from(reticulate, array_reshape)
# nd <- c(10, 512)
#
# zz <- list(c(10, 512), rev(d), c(128, 20, 2), prod(d))
#
# for(nd in zz) {
#   print(nd)
#   stopifnot(identical(
#     array_reshape(x, nd),
#     array_reshape2(x, nd)
#   ))
# }
#
# r <- microbenchmark::microbenchmark(
#   array_reshape(x, nd),
#   reshape2(x, nd)
# )


#
# # equivelant to reticulate::array_reshape(),
# # but a pure R solution (and therefore usually faster)
# array_reshape2 <- function(x, dim, order = c("C", "F")) {
#
#   # rename to avoid possible recursive loop when calling dim()
#   # arg is named `dim` for compatability with reticulate::array_reshape()
#   .dim <- dim; rm(dim)
#
#   order <- match.arg(order)
#   if (identical(order, "C"))
#     dim2(x) <- .dim
#   else
#     dim(x) <- .dim
#
#   x
# }
