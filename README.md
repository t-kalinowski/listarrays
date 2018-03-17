
<!-- README.md is generated from README.Rmd. Please edit that file -->
listarrays
==========

A toolbox for working with R arrays in a functional programming style. The core feature is a family of functions for flexibly splitting an array into lists of smaller arrays and combining a list of arrays into a single array. It also provides a set of lower-level helpers that abstract out patterns commonly encountered while working with arrays, as well as pipe-friendly and more flexible versions of `dim<-`() and `dimnames<-`(). The majority of the functions are agnostic about the number of dimmensions in the provided array and work recursively, enabling operations on multiple arrays to be performd synchronysly. Listarrays has no dependancies, but it is designed to work well with the packages purrr and magrittr.

Installation
------------

listarrays is not on CRAN yet. You can install the development version from github with

``` r
devtools::install_github("t-kalinowski/listarrays")
```

The package provides:

-   A family of functions for spliting, binding, and modifying multi-dimensional arrays.

    -   `split_on_dim()` and `split_along_dim()` take an array and returns a list.

    -   `bind_on_dim()` and `bind_as_dim()` take a list and return a single array.

    -   `modify_along_dim()` takes an array, calls the passed function `.f()` on each subset of the specified dimension, and returns an array of the same shape.

    -   Many of the functions have a varient `*_rows()` for the most common case of the 1st dimension. For example `split_on_rows()` which is equivalent to `split_on_dim(X, 1)`

-   `set_dim()` and `set_dimnames()`, pipe-friendly and more flexible versions of `dim<-` and `dimnames<-`

-   A family of lower-level helpers that abstract out patterns commonly encountered while working with arrays, for example `seq_along_rows()` (a combination of `seq_along()` and `nrow()`, with some error checks for common pitfalls).

-   Many of the functions are agnostic about the number of dimensions in the array passed. In practice, this means that you can use the same code path for arrays of various sizes. For example, to extract `idx` on the 1st dimension, you can use `extract_rows(X, idx)`, and it will work for an array of any number of dimensions. Contrast this with the base alternative `X[idx,,]`, where you have to modify the number of commas `,` to match the number of dimensions in `X`.

-   Many of the functions work recursively if provided a list of arrays. This is particularly useful in combination with `` zeallot::`%<-%`() `` multi-assignment operator, which allows for patterns like:

    ``` r
    c(X, Y) %<-% shuffle_rows(X, Y, in_sync = TRUE)
    ```

-   A set of functions that help encode atomic vectors as `onehot()` binary matrix's and `decode_onehot()` back into atomic vectors.
