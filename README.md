
<!-- README.md is generated from README.Rmd. Please edit that file -->

# listarrays

[![CRAN
status](https://www.r-pkg.org/badges/version/listarrays)](https://cran.r-project.org/package=listarrays)

A toolbox for working with R arrays in a functional programming style.
Flexibly split, bind, reshape, modify, subset, and name arrays.

The package provides:

  - `split_on_dim()` and `split_along_dim()` which take an array and
    return a list.

  - `bind_on_dim()` and `bind_as_dim()` take a list and return an array.

  - `modify_along_dim()` takes an array, calls the passed function
    `.f()` on each subset of the specified dimension, and returns an
    array of the same shape. (think of this as a safer and sometimes
    faster alternative to `base::apply()` that is guaranteed to return
    an array of the same shape as it received)

  - `extract_dim()` a wrapper around `[` that allows you to specify the
    dimension being subset as a function argument. For example,
    `extract_dim(X, 1, idx)` will extract `idx` on the first dimension,
    regardless how many dimensions are in the array `X`. Contrast this
    with the base alternative `X[idx,,]`, where you have to match the
    number of commas `,` to the number of dimensions in `X`.

  - Many of the functions have two variants `*_rows()` and `*_cols()`
    for the two most common case of the first and last dimension. For
    example `split_on_rows()` which is equivalent to
    `split_on_dim(X, 1)` and `split_on_cols()` which is equivalent to
    `split_on_dim(X, -1)`

  - `set_dim()` and `set_dimnames()`, pipe-friendly and more flexible
    versions of `dim<-` and `dimnames<-`

  - `dim2()<-`, `set_dim2()`, `array2()`, which reshape or fills arrays
    using row-major (C-style) semantics

  - `t.array()` a transpose method for multi-dimensional arrays

  - A handful of lower-level helpers that abstract out patterns commonly
    encountered while working with arrays, for example `expand_dims()`
    (the inverse of `base::drop()`, or seq\_along\_rows()`(a combination
    of`seq\_along()`and`nrow()\`).

  - A set of functions that help encode atomic vectors as `onehot()`
    binary matrix’s and `decode_onehot()` back into atomic vectors. (for
    example if training a neural network with keras)

  - Many of the functions work recursively if provided a list of arrays.

## Installation

You can install listarrays from CRAN with:

``` r
install.packages("listarrays")
```

Or install the development version from github with:

``` r
devtools::install_github("t-kalinowski/listarrays")
```
