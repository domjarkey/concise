
<!-- README.md is generated from README.Rmd. Please edit that file -->

# concise

<!-- badges: start -->
<!-- badges: end -->

## Overview

Writing impromptu functions to transform your data is one of the most
common procedures in data science, but this can often lead to
complicated and confusing code, even when performing relatively simple
operations. `concise` gives you a set of tools to apply clean and
intelligible anonymous functions that transform your data while keeping
your code concise and easily readable. `concise` functions are modelled
on familiar `tidyverse` functions like `purrr::map` and `dplyr::mutate`,
but with a layer of syntactic sugar to make anonymous functions that can
condense a paragraph’s worth of code into a single line.

- Refer to data columns directly – avoid placeholder pronouns like `.x`
  or `..1` and instead refer to your data by name inside function
  definitions.
- Leverages `purrr`’s mapping functions to outperform slow
  `dplyr::rowwise` operations and facilitate non-rowwise column
  mutations in the same call.
- Helpful shorthand gives access to column properties like row number,
  the size of grouped data (i.e. cardinality) and the names attribute
  inside the iterative function.
- Interact with and preserve groups created by `dplyr::group_by`.
- Access the entire data column as well as individual elements to write
  custom summary and window functions such as moving averages.
- Refer to anonymous functions inside their own definitions to write
  recursive functions.

## Installation

You can install the development version of concise from
[GitHub](https://github.com/domjarkey/concise) with:

``` r
# install.packages("remotes")
remotes::install_github("domjarkey/concise")
```

## Usage

### `cmutate`

`cmutate` is equivalent to `dplyr`’s `mutate` but with the additional
option of evaluating column definitions as a lambda function iteratively
applied to each row. Columns defined using `~` instead of `=` will be
evaluated as an anonymous function with special `concise` syntax.

This allows for the easy application of non-vectorised functions on a
row-by-row basis.

``` r
library(concise)

# The is.null function is not vectorised, so normally can't be called on an entire
# column unless a function like purrr::map is used
tibble(fruit = list('apple', 'banana', NULL, 'dragonfruit', NULL)) |>
    cmutate(fruit_exists ~ !is.null(fruit))
#> # A tibble: 5 × 2
#>   fruit     fruit_exists
#>   <list>    <lgl>       
#> 1 <chr [1]> TRUE        
#> 2 <chr [1]> TRUE        
#> 3 <NULL>    FALSE       
#> 4 <chr [1]> TRUE        
#> 5 <NULL>    FALSE
```

As with `dplyr::mutate`, ordinary column mutations can also be called
with `=`, and multiple mutations can be performed at once, able to make
reference to columns created or modified within the same function call:

``` r
tibble(fruit = list('apple', 'banana', NULL, 'dragonfruit', NULL)) |>
    cmutate(
        fruit_exists ~ !is.null(fruit),
        fruit_name_length ~ ifelse(fruit_exists, stringr::str_length(fruit), 0),
        fruit = as.character(ifelse(fruit_exists, fruit, "NO FRUIT FOUND"))
    )
#> # A tibble: 5 × 3
#>   fruit          fruit_exists fruit_name_length
#>   <chr>          <lgl>                    <dbl>
#> 1 apple          TRUE                         5
#> 2 banana         TRUE                         6
#> 3 NO FRUIT FOUND FALSE                        0
#> 4 dragonfruit    TRUE                        11
#> 5 NO FRUIT FOUND FALSE                        0
```

#### Examples

##### Find the largest element of multiple columns:

`concise` handles expressions marked with a `~` using its own simplified
syntax, but behind the scenes it passes any columns called to `purrr`’s
`pmap` function. This makes it computationally faster and more versatile
than the equivalent operation using `dplyr::rowwise` while making your
source code easier to write and clearer to read.

``` r
# This data frame will be referred to in the next few examples
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
    avg_of_last_3_z_values ~ mean(z.col[max(.i - 3, 1):.i])
)
#> # A tibble: 10 × 4
#>        x     y     z avg_of_last_3_z_values
#>    <int> <int> <int>                  <dbl>
#>  1    29    38    31                   31  
#>  2    11    80    83                   57  
#>  3    72    98    91                   68.3
#>  4    81    93    69                   68.5
#>  5    27    34    82                   81.2
#>  6    61    26    65                   76.8
#>  7    42     4    75                   72.8
#>  8    26    31     3                   56.2
#>  9    57    18    20                   40.8
#> 10    39    69    71                   42.2
```

##### Use pronouns in combination with data groupings

When data is grouped, the `<column_name>.grp` pronoun refers to all
elements of the data in the same group as a vector, while
`<column_name>.col` will refer to the entire, ungrouped column.
Similarly, `.i` will refer to the index of the element in the group,
while `.I` will refer to the absolute row index.

``` r
numbers$letter <- rep(c('A', 'B'), each = 5)

numbers |>
    select(letter, x) |>
    group_by(letter) |>
    cmutate(
        prop_of_group ~ x / sum(x.grp),
        prop_of_whole ~ x / sum(x.col),
        group_row_index ~ .i,
        column_row_index ~ .I
    )
#> # A tibble: 10 × 6
#> # Groups:   letter [2]
#>    letter     x prop_of_group prop_of_whole group_row_index column_row_index
#>    <chr>  <int>         <dbl>         <dbl>           <int>            <int>
#>  1 A         29         0.132        0.0652               1                1
#>  2 A         11         0.05         0.0247               2                2
#>  3 A         72         0.327        0.162                3                3
#>  4 A         81         0.368        0.182                4                4
#>  5 A         27         0.123        0.0607               5                5
#>  6 B         61         0.271        0.137                1                6
#>  7 B         42         0.187        0.0944               2                7
#>  8 B         26         0.116        0.0584               3                8
#>  9 B         57         0.253        0.128                4                9
#> 10 B         39         0.173        0.0876               5               10
```

Note: Similar to `.i`/`.I`, when data is grouped, `.n` refers to size of
the group, while `.N` refers to the total number of rows in the entire
data frame.

##### Specifying data type with `?`

By default, `cmutate` simplifies the output to the most suitable data
type. On occasion, it may be necessary to explicitly specify the data
type of the output column, similar to calling `map_int` or `map_chr`.
This can be done using the `?` operator as in the following example:

``` r
numbers |>
    select(x, y, z) |>
    cmutate(
        max ~ max(x, y, z),
        max_int ~ max(x, y, z) ? int,
        max_dbl ~ max(x, y, z) ? dbl,
        max_chr ~ max(x, y, z) ? chr,
        max_list ~ max(x, y, z) ? list,
    )
#> # A tibble: 10 × 8
#>        x     y     z   max max_int max_dbl max_chr max_list 
#>    <int> <int> <int> <int>   <int>   <dbl> <chr>   <list>   
#>  1    29    38    31    38      38      38 38      <int [1]>
#>  2    11    80    83    83      83      83 83      <int [1]>
#>  3    72    98    91    98      98      98 98      <int [1]>
#>  4    81    93    69    93      93      93 93      <int [1]>
#>  5    27    34    82    82      82      82 82      <int [1]>
#>  6    61    26    65    65      65      65 65      <int [1]>
#>  7    42     4    75    75      75      75 75      <int [1]>
#>  8    26    31     3    31      31      31 31      <int [1]>
#>  9    57    18    20    57      57      57 57      <int [1]>
#> 10    39    69    71    71      71      71 71      <int [1]>
```

<!-- N.B. As with `purrr`'s mapping functions, `?` won't automatically coerce any data -->
<!-- type, so it is recommended to use functions such as `as.integer` or `as.character` -->
<!-- where appropriate. -->

### `rmap` and `cmap`

`rmap` (short for “row map”) applies an anonymous function to the rows
of a data frame while allowing the columns to be referred to directly
inside the function definition. As with `cmutate` and other `concise`
functions, pronouns such as `.i` (the index or position in the element
in the list) are able to be used.

`rmap` works similarly to `purrr::pmap` except the input data frame does
not need to be subsetted to only those columns used in the function. By
default, `rmap` returns a list, but the data type of the output vector
can also specified with a suffix in a similar fashion to other `purrr`
map functions, e.g. `rmap_chr`, `rmap_dbl`, `rmap_df`, etc.

``` r
numbers |>
    rmap(~ paste0("\n\tRow ", .i, ", Group ", letter, ": ", mean(c(x, y, z)))) |>
    cat()
#> 
#>  Row 1, Group A: 32.6666666666667 
#>  Row 2, Group A: 58 
#>  Row 3, Group A: 87 
#>  Row 4, Group A: 81 
#>  Row 5, Group A: 47.6666666666667 
#>  Row 6, Group B: 50.6666666666667 
#>  Row 7, Group B: 40.3333333333333 
#>  Row 8, Group B: 20 
#>  Row 9, Group B: 31.6666666666667 
#>  Row 10, Group B: 59.6666666666667
```

`cmap` is equivalent to `purrr::map`, applying an anonymous function
iteratively to a list input, except it also allows for use of `concise`
pronouns. In the below example, `.nm` is used to refer to the `names`
attribute for each element in the input vector, and `.col` refers to the
entire input vector.

``` r
state_areas <- state.area
names(state_areas) <- state.name

head(state_areas)
#>    Alabama     Alaska    Arizona   Arkansas California   Colorado 
#>      51609     589757     113909      53104     158693     104247
```

``` r

state_areas |>
    cmap_df(
        ~ c(
            state = .nm,
            larger_than_median_state = .x > median(.col)
        )
    )
#> # A tibble: 50 × 2
#>    state       larger_than_median_state
#>    <chr>       <chr>                   
#>  1 Alabama     FALSE                   
#>  2 Alaska      TRUE                    
#>  3 Arizona     TRUE                    
#>  4 Arkansas    FALSE                   
#>  5 California  TRUE                    
#>  6 Colorado    TRUE                    
#>  7 Connecticut FALSE                   
#>  8 Delaware    FALSE                   
#>  9 Florida     TRUE                    
#> 10 Georgia     TRUE                    
#> # ℹ 40 more rows
```

Note: If the column of an input data frame is a named vector, the
`<column_name>.nm` pronoun can be used in `rmap` and `cmutate` function
definitions in a similar fashion.

### Recursion in `concise`

It can sometimes be useful to define a function recursively, performing
the same computation on an output until a base case is reached. This is
not always the advisable, but in certain circumstances such as list
traversal or web scraping, it can be handy to call a function inside
itself to complete a task with an indeterminate amount of steps. In
`concise` function definitions, this can be accomplished using the
`.this` pronoun.

The canonical example of recursion is Fibonacci’s sequence, where the
first two terms are defined as 1 and 1 (or sometimes 0 and 1), and the
nth term is defined as the sum of the two previous terms. This sequence
can be succinctly computed using `cmap`:

``` r
cmap_int(1:10, ~ if (.x <= 2) {1} else {.this(.x - 1) + .this(.x - 2)})
#>  [1]  1  1  2  3  5  8 13 21 34 55
```

Tree-like structures or nested lists can be traversed recursively to
extract leaf nodes or to collapse them into a standardised format.

``` r
tree <- list(
    a = list(
        b = 1,
        c = list(
            d = 2,
            e = 3
        )
    ),
    f = 4,
    g = list(h = 5)
)

cmap_df(
    tree,
    ~ if (is.list(.x)) {
        purrr::pmap_df(
            list(.x, .nm = names(.x)),
            .this,
            path = paste0(path, "/", .nm)
        )
    } else {
        tibble(path = paste0(path, "/", .nm), value = .x)
    },
    path = "root"
)
#> # A tibble: 5 × 2
#>   path       value
#>   <chr>      <dbl>
#> 1 root/a/b       1
#> 2 root/a/c/d     2
#> 3 root/a/c/e     3
#> 4 root/f         4
#> 5 root/g/h       5
```

### `concise` infix operators

The `concise` package also includes three infix operators designed to be
used to make applying key-value or dictionary lookups easier to use and
more understandable for readers of your code. These infixes provide a
straightforward syntax for mapping from one set to another as an
alternative to relying on joins and cross-walk datasets.

`%from%` and `%to%` are designed to work as a pair of ternary operators,
mapping and input vector *from* a set of keys *to* a set of values.

``` r
# Map a sequence of letters to their numerical positions in the alphabet
c('d', 'o', 'g') %from% letters %to% 1:26
#> [1]  4 15  7
```

``` r
# Look up the abbreviations for US states
c('California', 'Virginia', 'Texas') %from% state.name %to% state.abb
#> [1] "CA" "VA" "TX"
```

The `%with%` operator allows a preceding expression to be evaluated
*with* a data.frame or named list object as the local environment. Below
we use `dplyr`’s `starwars` dataset as an example.

``` r
data("starwars", package = "dplyr")
head(starwars)
#> # A tibble: 6 × 14
#>   name      height  mass hair_color skin_color eye_color birth_year sex   gender
#>   <chr>      <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr> <chr> 
#> 1 Luke Sky…    172    77 blond      fair       blue            19   male  mascu…
#> 2 C-3PO        167    75 <NA>       gold       yellow         112   none  mascu…
#> 3 R2-D2         96    32 <NA>       white, bl… red             33   none  mascu…
#> 4 Darth Va…    202   136 none       white      yellow          41.9 male  mascu…
#> 5 Leia Org…    150    49 brown      light      brown           19   fema… femin…
#> 6 Owen Lars    178   120 brown, gr… light      blue            52   male  mascu…
#> # ℹ 5 more variables: homeworld <chr>, species <chr>, films <list>,
#> #   vehicles <list>, starships <list>
```

``` r
# Find mean height of characters in the starwars dataset
mean(height, na.rm = TRUE) %with% starwars
#> [1] 174.6049
```

`%with%` can even be used in tandem with `%from%` and `%to%`:

``` r
# Map character names to species using the starwars dataset
c("Han Solo", "R2-D2", "Chewbacca") %from% name %to% species %with% starwars
#> [1] "Human"   "Droid"   "Wookiee"
```
