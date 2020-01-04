#' Set dimnames
#'
#' A more flexible and pipe-friendly version of `dimnames<-`.
#'
#' @param x an array
#' @param nm A list or character vector.
#' @param which_dim a character vector or numeric vector or `NULL`
#'
#' @details This function is quite flexible. See examples for the complete
#'   picture.
#'
#' @note The word "dimnames" is slightly overloaded. Most commonly it refers to
#'   the names of entries along a particular axis (e.g., date1, date2, date3,
#'   ...), but occasionally it is also used to refer to the names of the array
#'   axes themselves (e.g, dates, temperature, pressure, ...). To disambiguate,
#'   in the examples 'dimnames' always refers to the first case, while 'axis
#'   names' refers to the second. `set_dimnames()` can be used to set either or both
#'   axis names and dimnames.
#'
#' @return x, with modified dimnames and or axisnames
#' @export
#' @importFrom utils modifyList
#'
#' @examples
#' x <- array(1:8, 2:4)
#'
#' # to set axis names, leave which_dim=NULL and pass a character vector
#' dimnames(set_dimnames(x, c("a", "b", "c")))
#'
#' # to set names along a single axis, specify which_dim
#' dimnames(set_dimnames(x, c("a", "b", "c"), 2))
#'
#' # to set an axis name and names along the axis, pass a named list
#' dimnames(set_dimnames(x, list(axis2 = c("a", "b", "c")), 2))
#' dimnames(set_dimnames(x, list(axis2 = c("a", "b", "c"),
#'                               axis3 = 1:4), which_dim = 2:3))
#'
#' # if the array already has axis names, those are used when possible
#' nx <- set_dimnames(x, paste0("axis", 1:3))
#' dimnames(nx)
#' dimnames(set_dimnames(nx, list(axis2 = c("x", "y", "z"))))
#' dimnames(set_dimnames(nx, c("x", "y", "z"), which_dim = "axis2"))
#'
#'
#' # pass NULL to drop all dimnames, or just names along a single dimension
#' nx2 <- set_dimnames(nx, c("x", "y", "z"), which_dim = "axis2")
#' nx2 <- set_dimnames(nx2, LETTERS[1:4], which_dim = "axis3")
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
set_dimnames <- function(x, nm, which_dim = NULL) {
  if (is.null(nm))
    return(drop_dimnames(x, which_dim))

  else if (is.list(nm)) {
    nm <- lapply(nm, as.character)
    if (identical(nm, list(character())))
      return(drop_dimnames(x, which_dim, keep_axis_names = TRUE))
  } else
    nm <- as.character(nm)

  dim_nms <- dimnames(x) %||% vector("list", ndim(x))

  if (is.null(which_dim)) {
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
  } else { # which_dim supplied
    if(is.character(which_dim)) {
      stopifnot(all(which_dim %in% names(dimnames(x))))
      which_dim <- match(which_dim, names(dimnames(x)))
    }

      if (is.character(nm)) {
        stopifnot(identical(length(which_dim), 1L),
                  identical(length(nm), dim(x)[which_dim]))
        dimnames(x)[[which_dim]] <- nm
        return(x)

      } else { # nm is a list and dim supplied

        stopifnot(identical(length(nm), length(which_dim)))
        for (i in seq_along(nm)) {
          dim_nms[[which_dim[i]]] <- nm[[i]]
        }

        if(!is.null(names(nm))) {
          new_axis_nms <- names(dim_nms) %||% character(length(dim_nms))
          new_axis_nms[which_dim] <- names(nm)
          new_axis_nms[!nzchar(new_axis_nms)] <-
            names(dim_nms)[!nzchar(new_axis_nms)] %||% ""
          names(dim_nms) <- new_axis_nms
        }

        dimnames(x) <- dim_nms
        return(x)

        } # end nm is a list and dim supplied

    } # end else which_dim supplied
  stop("invalid input")
}


#' Drop dimnames
#'
#' A pipe-friendly wrapper for `dim(x) <- NULL` and `dimnames(x) <- NULL` or, if
#' `which_dim` is not `NULL`, \code{dimnames(x)[which_dim] <- list(NULL)}
#'
#' @param x an object, potentially with dimnames
#' @param which_dim If `NULL` (the default) then all dimnames are dropped. If
#'   integer vector, then dimnames only at the specified dimensions are dropped.
#' @param keep_axis_names TRUE or FALSE, whether to preserve the axis names when
#'   dropping the dimnames
#'
#' @export
drop_dimnames <- function(x, which_dim = NULL, keep_axis_names = FALSE) {
  if(is.null(which_dim)) {
    if(keep_axis_names)
      dimnames(x) <- lapply(dimnames(x), function(...) NULL)
    else
      dimnames(x) <- NULL
  } else {
    which_dim <- standardize_which_dim(which_dim, x, multiple_OK = TRUE)
    dimnames(x)[which_dim] <- list(NULL)
    if(!keep_axis_names)
      names(dimnames(x))[which_dim] <- ""
  }
  x
}

#' @rdname drop_dimnames
#' @export
drop_dim <- function(x) {
  dim(x) <- NULL
  x
}

#' @rdname drop_dimnames
#' @export
drop_dim2 <- function(x) {
  dim2(x) <- NULL
  x
}
