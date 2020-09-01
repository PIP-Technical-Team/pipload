
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
pip_data_find(
             country = "PRY",
             year    = c(2017, 2018),
             tool    = "PC"
)
#>                                                                                                                        orig
#> 1: //w1wbgencifs01/pip/PIP-Data/PRY/PRY_2017_EPH/PRY_2017_EPH_V01_M_V01_A_PIP/Data/PRY_2017_EPH_V01_M_V01_A_PIP_PC-GPWG.dta
#> 2: //w1wbgencifs01/pip/PIP-Data/PRY/PRY_2018_EPH/PRY_2018_EPH_V01_M_V01_A_PIP/Data/PRY_2018_EPH_V01_M_V01_A_PIP_PC-GPWG.dta
#>                                    filename country_code year survey_acronym
#> 1: PRY_2017_EPH_V01_M_V01_A_PIP_PC-GPWG.dta          PRY 2017            EPH
#> 2: PRY_2018_EPH_V01_M_V01_A_PIP_PC-GPWG.dta          PRY 2018            EPH
#>    vermast veralt collection  module tool source
#> 1:     v01    v01        PIP PC-GPWG   PC   GPWG
#> 2:     v01    v01        PIP PC-GPWG   PC   GPWG
```
