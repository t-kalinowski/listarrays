

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
#' @details This function is quite flexible. See examples for the complete
#'   picture.
#'
#' @note The word "dimnames" is slightly overloaded. Most commonly it refers to
#'   the names of entries along a particular axis (e.g., date1, date2, date3,
#'   ...), but occausionally it is also used to refer to the names of the array
#'   axes themselves (e.g, dates, temperature, pressure, ...). To disambiguate,
#'   in the examples 'dimnames' always refers to the first case, while 'axis
#'   names' refers to the second. This function can be used either or both both
#'   axis names and dimnames.
#'
#' @return x, with dimnames
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
#' nx2 <- set_dimnames(nx2, LETTERS[1:4], .dim = "axis3")
#' dimnames(nx2)
#' dimnames(set_dimnames(nx2, NULL))
#' dimnames(set_dimnames(nx2, NULL, 2))
#' dimnames(set_dimnames(nx2, NULL, c(2, 3)))
#' # to preserve an axis name and only drop the dimnames, wrap the NULL in a list()
#' dimnames(set_dimnames(nx2, list(NULL)))
#' dimnames(set_dimnames(nx2, list(NULL), 2))
#' dimnames(set_dimnames(nx2, list(axis2 = NULL)))
#' dimnames(set_dimnames(nx2, list(axis2 = NULL, axis3 = NULL)))
#' dimnames(set_dimnames(nx2, list(NULL), 2:3))
set_dimnames <- function(x, nm, .dim = NULL) {
  if (is.null(nm))
    return(drop_dimnames(x, .dim))

  else if (is.list(nm)) {
    nm <- lapply(nm, as.character)
    if (identical(nm, list(character())))
      return(drop_dimnames(x, .dim, keep_axis_names = TRUE))
  } else
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



drop_dimnames <- function(x, .dim = NULL, keep_axis_names = FALSE) {
    .dim <-
  if(is.null(.dim)) {
    if(keep_axis_names)
      dimnames(x) <- lapply(dimnames(x), function(...) NULL)
    else
      dimnames(x) <- NULL
  } else {
    dimnames(x)[.dim] <- list(NULL)
    if(!keep_axis_names)
      names(dimnames(x))[.dim] <- ""
  }
  x
}
#
# .dim <- c(2, 3)
# x <- nx2
# # x <- set_dimnames(x, LETTERS[1:4], 3)
# dimnames(x)
# dimnames(x)[.dim] <- list(NULL)
# names(dimnames(x))[.dim] <- ""
# dimnames(x)

# ' @export
# get_dimnames <- function(x, .dim = NULL) {
#   if(is.null(.dim))
#     dimnames(x)
#   else if (length(.dim)  > 1L)
#     dimnames(x)[.dim]
#   else
#     dimnames(x)[[.dim]]
# }

