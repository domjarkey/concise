
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

This allows for the easy application of non-vectorised functions on a
row-by-row basis.

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

#### Examples

Find the largest value out of three columns…

``` r
df <- tibble::tibble(
    x = c(29L, 11L, 72L, 81L, 27L, 61L, 42L, 26L, 57L, 39L),
    y = c(38L, 80L, 98L, 93L, 34L, 26L, 4L, 31L, 18L, 69L),
    z = c(31L, 83L, 91L, 69L, 82L, 65L, 75L, 3L, 20L, 71L)
)
```

<div id="itcvgajlzv" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="x">x</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="y">y</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="x" class="gt_row gt_left"><div class='gt_from_md'><pre><code class="language-r">library(concise)
df |&gt; cmutate(
    largest ~ max(x, y, z)
)
#&gt; # A tibble: 10 × 4
#&gt;        x     y     z largest
#&gt;    &lt;int&gt; &lt;int&gt; &lt;int&gt;   &lt;int&gt;
#&gt;  1    29    38    31      38
#&gt;  2    11    80    83      83
#&gt;  3    72    98    91      98
#&gt;  4    81    93    69      93
#&gt;  5    27    34    82      82
#&gt;  6    61    26    65      65
#&gt;  7    42     4    75      75
#&gt;  8    26    31     3      31
#&gt;  9    57    18    20      57
#&gt; 10    39    69    71      71
</code></pre>
</div></td>
<td headers="y" class="gt_row gt_left"><div class='gt_from_md'><pre><code class="language-r">library(tidyverse)
df |&gt; mutate(
    largest = pmap_int(
        list(x, y, z),
        \\(x, y, z) max(x, y, z)
    )
)
#&gt; # A tibble: 10 × 4
#&gt;        x     y     z largest
#&gt;    &lt;int&gt; &lt;int&gt; &lt;int&gt;   &lt;int&gt;
#&gt;  1    29    38    31      38
#&gt;  2    11    80    83      83
#&gt;  3    72    98    91      98
#&gt;  4    81    93    69      93
#&gt;  5    27    34    82      82
#&gt;  6    61    26    65      65
#&gt;  7    42     4    75      75
#&gt;  8    26    31     3      31
#&gt;  9    57    18    20      57
#&gt; 10    39    69    71      71
</code></pre>
</div></td></tr>
  </tbody>
  &#10;  
</table>
</div>

### `rmap`

`rmap` works similarly to `purrr::pmap` except the input data frame does
not need to be subset to only those columns used in the function, and
the data columns can be directly referred to in the anonymous function.
