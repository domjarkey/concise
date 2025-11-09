---
title: concise
format:
  gfm:
    variant: +yaml_metadata_block
    preserve-tabs: true
editor: visual
---


<!-- badges: start -->

<!-- CRAN status (uncomment once on CRAN)
[![CRAN status](https://www.r-pkg.org/badges/version/concise)](https://CRAN.R-project.org/package=concise)
-->

[![R-CMD-check](https://github.com/domjarkey/concise/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/domjarkey/concise/actions)
[![License:
MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE.md)
<!-- Coverage (if/when enabled)
[![Codecov](https://codecov.io/gh/domjarkey/concise/branch/main/graph/badge.svg)](https://app.codecov.io/gh/domjarkey/concise)
--> <!-- Lifecycle (adjust as appropriate)
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)
--> <!-- badges: end -->

# Overview

**concise** helps you write **compact, readable, row-aware
transformations** in R by extending familiar tidyverse verbs with a `.`
suffix. These variants let you define **inline anonymous functions** for
per-row or grouped operations without the boilerplate of `rowwise()` or
one-off `map()` calls.

- **Familiar, extended verbs.** `mutate.` (and related verbs) mirror
  `dplyr` syntax but add first-class support for lambda (`~`)
  expressions that evaluate **row-by-row**.  
- **Name columns directly.** Inside a lambda you refer to columns **by
  name** (no `.x`/`..1` placeholders).  
- **Purrr-powered under the hood.** Lambda columns are evaluated via
  `purrr` mapping, offering a clean alternative to ad-hoc `rowwise()`
  code.  
- **Contextual “pronouns”.** Access row index, group size, and whole
  columns (e.g. for simple window functions) with lightweight
  shorthand.  
- **Group-aware.** Works with `dplyr::group_by()`—transformations
  respect existing group structure.

> **TL;DR**: If you’ve ever written a few awkward lines of `rowwise()` +
> `mutate()` or `purrr::pmap()` just to compute a custom per-row value,
> `concise` lets you do the same job **clearly in one step**.

# Installation

``` r
# Development version from GitHub
# install.packages("remotes")
remotes::install_github("domjarkey/concise")
```
