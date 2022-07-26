
<!-- README.md is generated from README.Rmd. Please edit that file -->

# desta

The goal of desta is to provide convenient access to The Design of Trade
Agreements Database (DESTA).

## Installation

You can install the development version of desta from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("pachadotdev/desta")
```

## Example

This is a basic example which shows you how to see the Chile-US dyad:

``` r
library(desta)
library(dplyr)

treaties_dyads %>% 
  filter(
    country1 == "Chile",
    country2 == "United States",
    year >= 2000
  )
#> # A tibble: 2 × 16
#>   country1 country2  iso1  iso2 number base_treaty name  entry_type consolidated
#>   <chr>    <chr>    <int> <int> <chr>        <int> <chr> <chr>             <int>
#> 1 Chile    United …   152   840 218            218 Chil… base_trea…            0
#> 2 Chile    United …   152   840 899            899 Tran… base_trea…            0
#> # … with 7 more variables: year <int>, entryforceyear <int>, language <chr>,
#> #   typememb <int>, regioncon <chr>, wto_listed <int>, wto_name <chr>
```

All the tables within the package are documented. Check the package
documentation from RStudio help pane.
