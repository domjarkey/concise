
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

<div style="display: flex;">

<div>

`concise` syntax:

``` r
library(concise)
df |> cmutate(
    largest ~ max(x, y, z)
)
#> # A tibble: 10 × 4
#>        x     y     z largest
#>    <int> <int> <int>   <int>
#>  1    29    38    31      38
#>  2    11    80    83      83
#>  3    72    98    91      98
#>  4    81    93    69      93
#>  5    27    34    82      82
#>  6    61    26    65      65
#>  7    42     4    75      75
#>  8    26    31     3      31
#>  9    57    18    20      57
#> 10    39    69    71      71
```

</div>

<div>

Equivalent code:

``` r
library(tidyverse)
df |> mutate(
    largest = pmap_int(
        list(x, y, z),
        \(x, y, z) max(x, y, z)
    )
)
#> # A tibble: 10 × 4
#>        x     y     z largest
#>    <int> <int> <int>   <int>
#>  1    29    38    31      38
#>  2    11    80    83      83
#>  3    72    98    91      98
#>  4    81    93    69      93
#>  5    27    34    82      82
#>  6    61    26    65      65
#>  7    42     4    75      75
#>  8    26    31     3      31
#>  9    57    18    20      57
#> 10    39    69    71      71
```

</div>

</div>

<div id="lrjkuvqqyh" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#lrjkuvqqyh table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#lrjkuvqqyh thead, #lrjkuvqqyh tbody, #lrjkuvqqyh tfoot, #lrjkuvqqyh tr, #lrjkuvqqyh td, #lrjkuvqqyh th {
  border-style: none;
}
&#10;#lrjkuvqqyh p {
  margin: 0;
  padding: 0;
}
&#10;#lrjkuvqqyh .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#lrjkuvqqyh .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#lrjkuvqqyh .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#lrjkuvqqyh .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#lrjkuvqqyh .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#lrjkuvqqyh .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#lrjkuvqqyh .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#lrjkuvqqyh .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#lrjkuvqqyh .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#lrjkuvqqyh .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#lrjkuvqqyh .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#lrjkuvqqyh .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#lrjkuvqqyh .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#lrjkuvqqyh .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#lrjkuvqqyh .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#lrjkuvqqyh .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#lrjkuvqqyh .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#lrjkuvqqyh .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#lrjkuvqqyh .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lrjkuvqqyh .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#lrjkuvqqyh .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#lrjkuvqqyh .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#lrjkuvqqyh .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lrjkuvqqyh .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#lrjkuvqqyh .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#lrjkuvqqyh .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#lrjkuvqqyh .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lrjkuvqqyh .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#lrjkuvqqyh .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#lrjkuvqqyh .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#lrjkuvqqyh .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#lrjkuvqqyh .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#lrjkuvqqyh .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lrjkuvqqyh .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#lrjkuvqqyh .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lrjkuvqqyh .gt_left {
  text-align: left;
}
&#10;#lrjkuvqqyh .gt_center {
  text-align: center;
}
&#10;#lrjkuvqqyh .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#lrjkuvqqyh .gt_font_normal {
  font-weight: normal;
}
&#10;#lrjkuvqqyh .gt_font_bold {
  font-weight: bold;
}
&#10;#lrjkuvqqyh .gt_font_italic {
  font-style: italic;
}
&#10;#lrjkuvqqyh .gt_super {
  font-size: 65%;
}
&#10;#lrjkuvqqyh .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#lrjkuvqqyh .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#lrjkuvqqyh .gt_indent_1 {
  text-indent: 5px;
}
&#10;#lrjkuvqqyh .gt_indent_2 {
  text-indent: 10px;
}
&#10;#lrjkuvqqyh .gt_indent_3 {
  text-indent: 15px;
}
&#10;#lrjkuvqqyh .gt_indent_4 {
  text-indent: 20px;
}
&#10;#lrjkuvqqyh .gt_indent_5 {
  text-indent: 25px;
}
</style>
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
        \(x, y, z) max(x, y, z)
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
