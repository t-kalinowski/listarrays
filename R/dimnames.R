

# a more flexible and pipe-friendly version of dimnames<- Compared to dimnames<-
#
# +  if passed a single character vector and the character vector is of length
# dim(x), and .dim = NULL, then the names are assigned to the dimensions
#
# + allows you to set a single or just a few dimensions only, by specifying
# ".dim". if setting just one, then nm can be either a character vector (or
# something coercible to character) and must be the same length as the dim being
# set. Alternatively, it can be a list which can be named, and in turn contains
# a list of character vectors.


#' Set dimnames
#'
#' A more flexible and pipe-friendly version of `dimnames<-`.
#'
#' @param x an array
#' @param nm A list or character vector.
#' @param .dim a character vector or numeric vector or `NULL`
#'
#' @details The word "dimnames" is overloaded, in that it can refer to either
#'   the names of the array axes (e.g, metric, timestep, channels), or it can
#'   refer to the names of entries along a particular axes (e.g., date1, date2,
#'   date3, ...). This function can be used to set either one (by passing `nm` a
#'   character vector and specifying `.dim` accordingly), or both at the same
#'   time (by passing a named list to `nm`).
#'
#'
#' @return
#' x, with dimnames
#' @export
#' @importFrom utils modifyList
#'
#' @examples
#' x <- array(1:8, 2:4)
#'
#' # to set axis names, leave .dim=NULL and pass a character vector
#' dimnames(set_dimnames(x, c("a", "b", "c")))
#'
#' # to set names along a single axis, specify .dim
#' dimnames(set_dimnames(x, c("a", "b", "c"), 2))
#'
#' # to set an axis name and names along the axis, pass a named list
#' dimnames(set_dimnames(x, list(axis2 = c("a", "b", "c")), 2))
#' dimnames(set_dimnames(x, list(axis2 = c("a", "b", "c"),
#'                               axis3 = 1:4), .dim = 2:3))
#'
#' # if the array already has axis names, those are used when possible
#' nx <- set_dimnames(x, paste0("axis", 1:3))
#' dimnames(nx)
#' dimnames(set_dimnames(nx, list(axis2 = c("x", "y", "z"))))
#' dimnames(set_dimnames(nx, c("x", "y", "z"), .dim = "axis2"))
#'
#'
#' # pass NULL to drop all dimnames, or just names along a single dimension
#' nx2 <- set_dimnames(nx, c("x", "y", "z"), .dim = "axis2")
#' dimnames(nx2)
#' dimnames(set_dimnames(nx2, NULL))
#' dimnames(set_dimnames(nx2, NULL, 2L))
set_dimnames <- function(x, nm, .dim = NULL) {
  if (is.null(nm))
    return(drop_dimnames(x, .dim))
  else if (is.list(nm))
    nm <- lapply(nm, as.character)
  else
    nm <- as.character(nm)

  dim_nms <- dimnames(x) %||% vector("list", ndims(x))

  if (is.null(.dim)) {
    if (is.character(nm)) {
      stopifnot(identical(length(nm), length(dim(x))))
      names(dim_nms) <- nm
      dimnames(x) <- dim_nms
      return(x)

    } else if (is.list(nm)) {
      # if x dims are named and nm is a named list, match names
      if (!is.null(names(nm)) &&
          all(nzchar(names(nm))) &&
          all(names(nm) %in% names(dim_nms))) {
        dim_nms <- modifyList(dim_nms, nm, keep.null = TRUE)
        dimnames(x) <- dim_nms
        return(x)

      } else {
        stopifnot(length(nm) == dim(x))
        if (!is.null(names(nm)))
          warning("Names supplied to `nm` are ignored")
        dimnames(x) <- nm
        return(x)
      }
    }
  } else { # .dim supplied
    if(is.character(.dim)) {
      stopifnot(all(.dim %in% names(dimnames(x))))
      .dim <- match(.dim, names(dimnames(x)))
    }

      if (is.character(nm)) {
        stopifnot(identical(length(.dim), 1L),
                  identical(length(nm), dim(x)[.dim]))
        dimnames(x)[[.dim]] <- nm
        return(x)

      } else { # nm is a list and dim supplied

        stopifnot(identical(length(nm), length(.dim)))
        for (i in seq_along(nm)) {
          dim_nms[[.dim[i]]] <- nm[[i]]
        }

        if(!is.null(names(nm))) {
          new_axis_nms <- names(dim_nms) %||% character(length(dim_nms))
          new_axis_nms[.dim] <- names(nm)
          new_axis_nms[!nzchar(new_axis_nms)] <-
            names(dim_nms)[!nzchar(new_axis_nms)] %||% ""
          names(dim_nms) <- new_axis_nms
        }

        dimnames(x) <- dim_nms
        return(x)

        } # end nm is a list and dim supplied

    } # end else .dim supplied
  stop("invalid input")
} # end function definition



drop_dimnames <- function(x, .dim = NULL) {
  if(is.null(.dim))
    dimnames(x) <- NULL
  else
    for(d in .dim)
      dimnames(x)[[d]] <- NULL

    x
}



# ' @export
# get_dimnames <- function(x, .dim = NULL) {
#   if(is.null(.dim))
#     dimnames(x)
#   else if (length(.dim)  > 1L)
#     dimnames(x)[.dim]
#   else
#     dimnames(x)[[.dim]]
# }


#' Pipe friendly `dim<-`, with option to pad to necessary length
#'
#' @param x A vector to set dimensions on
#' @param .dim The desired dimensions
#' @param pad The value to pad the vector with. `NULL` (the default) performs no padding.
#' @param verbose Whether to emit a message if padding. By default, `FALSE`.
#'
#' @return Object with dimensions set
#' @export
#'
#' @examples
#' set_dim(1:10, c(2, 5))
#' try( set_dim(1:7, c(2, 5)) ) # error by default, just like `dim<-`()
#'      set_dim(1:7, c(2, 5), pad = 99)
set_dim <- function(x, .dim, pad = NULL, verbose = getOption("verbose")) {
  if (!is.null(pad) && !identical(length(x), needed_len <- prod(.dim))) {
    stopifnot(length(pad) == 1)
    if (verbose)
    message("Padding vector with ", pad, "s",
            " from length ", length(x), " to length ", needed_len)
    # expanding length of vector to ")
    x <- c(x, rep_len(pad, needed_len - length(x)))
  }

  dim(x) <- .dim
  x
}