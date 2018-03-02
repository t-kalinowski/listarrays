

# a more flexible and pipe-friendly version of dimnames<-
# Compared to dimnames<-
#
# +  if passed a single character vector and the character vector is of length
# dim(x), and .dim = NULL, then the names are assigned to the dimensions
#
# + allows you to set a single or just a few dimensions only, by specifying ".dim".
# if setting just one, then nm can be either a character vector (or something coercible to character) and must be the same length as the dim being set. Alternatively, it can be a list which can be named, and in turn contains a list of character vectors.


#' @export
set_dimnames <- function(x, nm, .dim = NULL) {
  if (is.list(nm))
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
        b()
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


#' @export
get_dimnames <- function(x, .dim = NULL) {
  if(is.null(.dim))
    dimnames(x)
  else if (length(.dim)  > 1L)
    dimnames(x)[.dim]
  else
    dimnames(x)[[.dim]]
}


#' @export
set_dim <- function(x,
                    .dim,
                    pad = NA,
                    verbose = TRUE) {
  if (!is.null(pad) && !identical(length(x), needed_len <- prod(.dim))) {
    if (verbose)
    message("Padding vector with ", pad, "s",
            " from length ", length(x), " to length ", needed_len)
    # expanding length of vector to ")
    length(x) <- needed_len
  }

  dim(x) <- .dim
  x
}