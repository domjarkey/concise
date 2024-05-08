
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

tibble(value = list('a', 'b', NULL, 'd', NULL)) |>
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
reference to columns created or modified within the same function call:

``` r
tidyr::expand_grid(
    word = c('banana', 'canal barge'),
    expression = c('.an', 'ba.')
) |> cmutate(
    substring ~ stringr::str_extract(word, expression),
    first_three = stringr::str_extract(word, "^\\w{3}"),
    concat = paste(first_three, substring)
)
#> # A tibble: 4 × 5
#>   word        expression substring first_three concat 
#>   <chr>       <chr>      <chr>     <chr>       <chr>  
#> 1 banana      .an        ban       ban         ban ban
#> 2 banana      ba.        ban       ban         ban ban
#> 3 canal barge .an        can       can         can can
#> 4 canal barge ba.        bar       can         can bar
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

##### Use pronouns in combination with data groupings

When data is grouped, the `<column_name>.grp` pronoun refers to all
elements of the data in the same group as a vector, while
`<column_name>.col` will refer to the entire, ungrouped column.
Similarly, `.i` will refer to the index of the elemenent in the group,
while `.I` will refer to the absolute index.

``` r
numbers$letter <- rep(c('A', 'B'), each = 5)

numbers |>
    select(letter, x) |>
    group_by(letter) |>
    cmutate(
        proportion_of_group ~ x / sum(x.grp),
        proportion_of_whole ~ x / sum(x.col),
        group_row_index ~ .i,
        columns_row_index ~ .I
    )
#> # A tibble: 10 × 6
#> # Groups:   letter [2]
#>    letter     x proportion_of_group proportion_of_whole group_row_index
#>    <chr>  <int>               <dbl>               <dbl>           <int>
#>  1 A         29               0.132              0.0652               1
#>  2 A         11               0.05               0.0247               2
#>  3 A         72               0.327              0.162                3
#>  4 A         81               0.368              0.182                4
#>  5 A         27               0.123              0.0607               5
#>  6 B         61               0.271              0.137                1
#>  7 B         42               0.187              0.0944               2
#>  8 B         26               0.116              0.0584               3
#>  9 B         57               0.253              0.128                4
#> 10 B         39               0.173              0.0876               5
#> # ℹ 1 more variable: columns_row_index <int>
```

N.B. Similarly to `.i`/`.I`, when grouped, `.n` refers to the row index
of the final entry in the group (or equally, the cardinality of the
group), and `.N` to the number of rows in the ungrouped data.

### `rmap`

`rmap` works similarly to `purrr::pmap` except the input data frame does
not need to be subset to only those columns used in the function, and
the data columns can be directly referred to in the anonymous function.
