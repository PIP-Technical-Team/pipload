
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pipload

<!-- badges: start -->

<!-- badges: end -->

The goal of `pipload` is to provide a series of tools to load into
memory the PIP microdata. You can load and update the inventory of PIP
microdata, as well as find the data most recent version of each
country-year-survey data point.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("PIP-Technical-Team/pipload")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(pipload)
## basic example code
df <- pip_data_find(
             country = "PRY",
             year    = c(2017, 2018),
             tool    = "PC"
)

df$filename
#> [1] "PRY_2017_EPH_V01_M_V01_A_PIP_PC-GPWG.dta"
#> [2] "PRY_2018_EPH_V01_M_V01_A_PIP_PC-GPWG.dta"
```
