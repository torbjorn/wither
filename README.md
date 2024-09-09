
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wither

<!-- badges: start -->

[![R-CMD-check](https://github.com/torbjorn/wither/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/torbjorn/wither/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Run a block or expression temporarily under a different `here()` root.

This is useful if you need to source R code from another project for
example (as might be the case with git submodules)

## Installation

You can install the development version of wither from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("torbjorn/wither")
```

## Example

### with\_here

`with_here()` evaluates an expression under a temporarily different here
root():

``` r
library(here)
library(wither)

was <- here()

d <- tempfile()
dir.create(d)

# have here() be somewhere else for an expression
is_now <- with_here(d, here())

stopifnot(normalizePath(was) != normalizePath(is_now))

# clean up
unlink(d, recursive=TRUE)
```

### local\_here

`local_here()` evaluates the remainder of a block under a temporarily
different here root():

``` r
library(here)
library(wither)

was <- here()

d <- tempfile()
dir.create(d)

local({

    # have here() be somewhere else for the rest of the block
    local_here(d)

    is_now <- here()

    stopifnot(normalizePath(was) != normalizePath(is_now))

})

# clean up
unlink(d, recursive=TRUE)
```
