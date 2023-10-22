
<!-- README.md is generated from README.Rmd. Please edit that file -->

# concise

<!-- badges: start -->
<!-- badges: end -->

## Overview

`concise` functions are designed to make clean, intelligible lambda
functions to keep your code concise. They are modelled on common
`tidyverse` functions like `purrr::map` and `dplyr::mutate`, but with a
layer of syntactic sugar to make anonymous functions that condense a
paragraph’s worth of code into a single line.

- Refer to data columns directly – avoid placeholder pronouns like `.x`
  or `..1` and instead refer to your data by name.
- Helpful shorthand gives access to vector properties like row number,
  vector length and the vector names inside the iterative function.
- Interact with and preserve groups created by `dplyr::group_by`.
- Access the entire data column as well as individual elements to write
  custom summary and window functions such as moving averages.
- Refer to anonymous functions inside their own definitions to write
  recursive functions.

## Installation

You can install the development version of concise from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("domjarkey/concise")
```

## Usage

### `cmutate`

`cmutate` performs the same role as `dplyr::mutate` but with the option
of evaluating column definitions as iterative lambda functions.

This allows for the easy use of non-vectorised functions.

``` r
library(concise)
library(dplyr)
library(purrr)

tibble(value = list("a", "b", NULL, "d", NULL)) |>
    cmutate(value_exists ~ !is.null(value))
#> # A tibble: 5 × 2
#>   value     value_exists
#>   <list>    <lgl>       
#> 1 <chr [1]> TRUE        
#> 2 <chr [1]> TRUE        
#> 3 <NULL>    FALSE       
#> 4 <chr [1]> TRUE        
#> 5 <NULL>    FALSE
```

This can side-step the problem of having to use mapping functions that
obfuscate the intent of your code. Consider the problem of picking the
largest element from amongst three columns. Naively, we might try a
simple application of the `max` function:

``` r
df <- tibble(
    x = c(29L, 11L, 72L, 81L, 27L, 61L, 42L, 26L, 57L, 39L),
    y = c(38L, 80L, 98L, 93L, 34L, 26L, 4L, 31L, 18L, 69L),
    z = c(31L, 83L, 91L, 69L, 82L, 65L, 75L, 3L, 20L, 71L)
)

df |> mutate(
    naive_largest = max(x, y, z)
)
#> # A tibble: 10 × 4
#>        x     y     z naive_largest
#>    <int> <int> <int>         <int>
#>  1    29    38    31            98
#>  2    11    80    83            98
#>  3    72    98    91            98
#>  4    81    93    69            98
#>  5    27    34    82            98
#>  6    61    26    65            98
#>  7    42     4    75            98
#>  8    26    31     3            98
#>  9    57    18    20            98
#> 10    39    69    71            98
```

Clearly this fails because `max` lumps the values of all the rows in
`x`, `y`, and `z` instead of evaluating the function row by row. We
might use `dplyr::rowwise` to overcome this problem, but this is an
undesirable alternative for a couple of reasons. First, `rowwise` is
very slow, especially for larger data frames. Second, `rowwise`
effectively works by grouping every row into its own separate group,
meaning existing groups are lost, and the data may need to be ungrouped
afterwards if further mutations need to be performed.

``` r
# More like slowwise
df |> 
    rowwise() |>
    mutate(rowwise_largest = max(x, y, z)) |>
    ungroup()
#> # A tibble: 10 × 4
#>        x     y     z rowwise_largest
#>    <int> <int> <int>           <int>
#>  1    29    38    31              38
#>  2    11    80    83              83
#>  3    72    98    91              98
#>  4    81    93    69              93
#>  5    27    34    82              82
#>  6    61    26    65              65
#>  7    42     4    75              75
#>  8    26    31     3              31
#>  9    57    18    20              57
#> 10    39    69    71              71
```

The better way to perform this operation that requires row-by-row
computation taking multiple data columns as inputs is with the
`purrr::pmap` function. `pmap` can either take a formula to specify the
expression of the lambda function, referring to each input column by its
position, or (in more recent versions of R) an anonymous function
defined using `\\\\(x, y, z) expr` notation. These look like this:

``` r
df |>
    mutate(
        pmap_largest_v1 = pmap_int(list(x, y, z), ~ max(..1, ..2, ..3)),
        pmap_largest_v2 = pmap_int(list(x, y, z), \(x, y, z) max(x, y, z))
    )
#> # A tibble: 10 × 5
#>        x     y     z pmap_largest_v1 pmap_largest_v2
#>    <int> <int> <int>           <int>           <int>
#>  1    29    38    31              38              38
#>  2    11    80    83              83              83
#>  3    72    98    91              98              98
#>  4    81    93    69              93              93
#>  5    27    34    82              82              82
#>  6    61    26    65              65              65
#>  7    42     4    75              75              75
#>  8    26    31     3              31              31
#>  9    57    18    20              57              57
#> 10    39    69    71              71              71
```

Of course these work a lot faster than `rowwise` (the second version is
fastest), but what they gain in speed they lose in brevity. Both end up
being much longer than might initially be expected for a relatively
simple operation, and the first is particularly hard to understand at a
glance as the `..x` pronouns obscure which variables they are actually
referring to. The purpose of the `cmutate` function is to avoid this
trade-off between speed and intelligibility, as can be seen below:

``` r
df |> cmutate(concise_largest ~ max(x, y, z))
#> # A tibble: 10 × 4
#>        x     y     z concise_largest
#>    <int> <int> <int>           <int>
#>  1    29    38    31              38
#>  2    11    80    83              83
#>  3    72    98    91              98
#>  4    81    93    69              93
#>  5    27    34    82              82
#>  6    61    26    65              65
#>  7    42     4    75              75
#>  8    26    31     3              31
#>  9    57    18    20              57
#> 10    39    69    71              71
```

### `rmap`

`rmap` works similarly to `purrr::pmap` except the input data frame does
not need to be subset to only those columns used in the function, and
the data columns can be directly referred to in the anonymous function.
