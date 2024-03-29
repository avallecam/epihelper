
<!-- README.md is generated from README.Rmd. Please edit that file -->

# epihelper

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/epihelper)](https://CRAN.R-project.org/package=epihelper)
[![Codecov test
coverage](https://codecov.io/gh/avallecam/epihelper/branch/master/graph/badge.svg)](https://codecov.io/gh/avallecam/epihelper?branch=master)
[![R-CMD-check](https://github.com/avallecam/epihelper/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/avallecam/epihelper/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of epihelper is to provide a miscelanea of useful custom
functions and related reproducible examples

## Installation

<!-- You can install the released version of epihelper from [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("epihelper") -->
<!-- ``` -->

And the development version from
[GitHub](https://github.com/avallecam/epihelper) with:

``` r
if(!require("remotes")) install.packages("remotes")
remotes::install_github("avallecam/epihelper")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(epihelper)
## basic example code
```

### initial helpers

- `adorn_ame`: adorn a
  [`tabyl`](https://cran.r-project.org/web/packages/janitor/vignettes/janitor.html#tabyl---a-better-version-of-table)
  with totals on margins, percentages and N on values in only one
  function!

- `print_inf`: make a quick(er) `print(n=Inf)`

- `read_lastfile`: read the last file in a folder (ideal for workflows
  with daily inputs and updates)

``` r
example("adorn_ame")
example("print_inf")
```

### spatial data management

- `read_gpx`: read GPX extension formats

- `st_coordinates_tidy`: a tidy alternative to `sf::st_coordinates`. it
  retrieve coordinates within the original sf/data.frame object.

- `sf_as_ppp`: integrates point geometry dataset and a boundary to
  create a ppp for spatstat analysis. [clink here for more
  information](https://github.com/r-spatial/sf/issues/1233).

- `tibble_as_raster`: transform a x,y,z tibble to a raster.

``` r
example("st_coordinates_tidy")
```

### movement data management

- `sum_range_h`: custom function to calculate amount of hours between to
  reported times

- `get_distance_m`: generates a distance output between two set of
  points within a tibble and flexible with `dplyr::group_by` and
  `purrr::pmap`

``` r
example("get_distance_m")
```

## Main functionalities

Check the [reference
page](https://avallecam.github.io/epihelper/reference/index.html) for
examples.

## Code of Conduct

Please note that the epihelper project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
