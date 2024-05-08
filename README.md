
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
- Leverages `purrr`’s mapping functions to outperform slow
  `dplyr::rowwise` operations and facilitate non-rowwise column
  mutations in the same call.
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

This allows for the easy application of non-vectorised functions on a
row-by-row basis.

``` r
library(concise)

tibble::tibble(value = list('a', 'b', NULL, 'd', NULL)) |>
    cmutate(value_exists ~ is.null(value))
#> # A tibble: 5 × 2
#>   value     value_exists
#>   <list>    <lgl>       
#> 1 <chr [1]> FALSE       
#> 2 <chr [1]> FALSE       
#> 3 <NULL>    TRUE        
#> 4 <chr [1]> FALSE       
#> 5 <NULL>    TRUE
```

As with `dplyr::mutate`, ordinary column mutations can also be called
with `=`, and multiple mutations can be called at once, able to make
reference to columns created within the same function call:

``` r
library(tidyverse)

expand_grid(
    word = c('banana', 'canal barge'),
    expression = c('.an', 'ba.')
) |> cmutate(
    substring ~ str_extract(word, expression),
    first_three = str_extract(word, "^\\w{3}"),
    concat = paste(word, first_three, sep = substring)
)
#> # A tibble: 4 × 5
#>   word        expression substring first_three concat           
#>   <chr>       <chr>      <chr>     <chr>       <chr>            
#> 1 banana      .an        ban       ban         bananabanban     
#> 2 banana      ba.        ban       ban         bananabanban     
#> 3 canal barge .an        can       can         canal bargebancan
#> 4 canal barge ba.        bar       can         canal bargebancan
```

#### Examples

##### Find the mean of multiple columns:

``` r
numbers <- tibble(
    x = c(29L, 11L, 72L, 81L, 27L, 61L, 42L, 26L, 57L, 39L),
    y = c(38L, 80L, 98L, 93L, 34L, 26L, 4L, 31L, 18L, 69L),
    z = c(31L, 83L, 91L, 69L, 82L, 65L, 75L, 3L, 20L, 71L)
)
```

<div align="center">

<img src="man/figures/table1.png" id="id" class="class" width="664"
height="448" />

</div>

##### Calculate a moving average of the three latest entries:

`concise` allows the use of special pronouns to refer to things like the
row index (`.i`), the final row index of the data (`.n`), or the entire
column as a vector (`<column_name>.col`). These can be used to make
simple, intelligible window functions.

``` r
numbers |> cmutate(
    avg_x ~ mean(x.col[max(.i - 3, 1):.i])
)
#> # A tibble: 10 × 4
#>        x     y     z avg_x
#>    <int> <int> <int> <dbl>
#>  1    29    38    31  29  
#>  2    11    80    83  20  
#>  3    72    98    91  37.3
#>  4    81    93    69  48.2
#>  5    27    34    82  47.8
#>  6    61    26    65  60.2
#>  7    42     4    75  52.8
#>  8    26    31     3  39  
#>  9    57    18    20  46.5
#> 10    39    69    71  41
```

### `rmap`

`rmap` works similarly to `purrr::pmap` except the input data frame does
not need to be subset to only those columns used in the function, and
the data columns can be directly referred to in the anonymous function.
