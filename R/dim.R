
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
set_dim <- function(x, .dim,
                    pad = getOption("listarrays.autopad_arrays_with", NULL),
                    order = c("F", "C"),
                    verbose = getOption("verbose")) {
  if (!is.null(pad) && !identical(length(x), needed_len <- prod(.dim))) {
    stopifnot(identical(length(pad), 1L))
    if (verbose)
      message("Padding vector with ", pad, "s",
              " from length ", length(x), " to length ", needed_len)
    x <- c(x, rep_len(pad, needed_len - length(x)))
  }

  order <- match.arg(order)
  if (identical(order, "C"))
    dim2(x) <- .dim
  else
    dim(x) <- .dim

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
  dx <- dim(x)
  if(identical(dx, as.integer(value)))
    return(x)

  if (!is.null(dx))
    x <- t(x)

  dim(x) <- rev(value)
  t(x)
}


# transpose
#' @export
t.array <- function(x) aperm(x, length(dim(x)):1)

# other candidate names
# dim_c()
# dim_rows()
# dim_r


# import::from(reticulate, array_reshape)
# 1:8 %>% array_reshape(c(4, 2))
# 1:8 %>% array_reshape(c(4, 2)) %>% array_reshape(c(4, 2))
# 1:8 %>% array_reshape(c(4, 2)) %>% array_reshape(c(2, 4))
# 1:8 %>% set_dim(c(4, 2)) %>% array_reshape(c(4, 2))
# 1:8 %>% array_reshape2(c(4, 2))
# 1:8 %>% array_reshape2(c(4, 2)) %>% array_reshape2(c(4, 2))
# 1:8 %>% array_reshape2(c(4, 2)) %>% array_reshape2(c(2, 4))
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



# equivelant to reticulate::array_reshape(),
# but a pure R solution (and therefore usually faster)
array_reshape2 <- function(x, dim, order = c("C", "F")) {

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
