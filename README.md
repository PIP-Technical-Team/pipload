
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

Load library

``` r
library(pipload)
```

### Microdata

Load Two datasets for Paraguay for the Poverty Calculator tool:

``` r
# Find the data available. 
df <- pip_find_data(
             country = "PRY",
             year    = c(2017, 2018),
             tool    = "PC"
)

df$filename
#> [1] "PRY_2017_EPH_V01_M_V01_A_PIP_PC-GPWG.dta"
#> [2] "PRY_2018_EPH_V01_M_V02_A_PIP_PC-GPWG.dta"

# load the data
df2 <- pip_find_data(
             country = "PRY",
             year    = c(2017, 2018),
             tool    = "PC"
)

names(df2)
#>  [1] "orig"           "filename"       "country_code"   "year"          
#>  [5] "survey_acronym" "vermast"        "veralt"         "collection"    
#>  [9] "module"         "tool"           "source"
```

### load Auxiliary data

Load different types of auxiliary data

``` r
# Load CPI
df <- pip_load_aux("cpi")
#> Most recent version of data loaded
head(df)
#>   country_code surveyid_year reference_year        cpi ccf cpi_domain
#> 1          AGO          2000        2000.21 0.03384806   1          1
#> 2          AGO          2008        2008.50 0.72333720   1          1
#> 3          AGO          2018        2018.17 3.06059498   1          1
#> 4          ALB          1996        1996.00 0.44443273   1          1
#> 5          ALB          2002        2002.00 0.78028772   1          1
#> 6          ALB          2005        2005.00 0.83847346   1          1
#>   cpi_data_level survey_acronym
#> 1       national            HBS
#> 2       national      IBEP-MICS
#> 3       national          IDREA
#> 4       national            EWS
#> 5       national           LSMS
#> 6       national           LSMS

# load PPP
df <- pip_load_aux("ppp")
#> Most recent version of data loaded
head(df)
#>   country_code ppp_year release_version adaptation_version      ppp ppp_default
#> 1          ABW     2005              v1                 v1       NA       FALSE
#> 2          ABW     2011              v1                 v1 1.652751       FALSE
#> 3          ABW     2011              v1                 v2 1.652751       FALSE
#> 4          ABW     2011              v2                 v1 1.637763       FALSE
#> 5          ABW     2011              v2                 v2 1.637763        TRUE
#> 6          ABW     2017              v1                 v1 1.480481       FALSE
#>   ppp_default_by_year ppp_domain ppp_data_level
#> 1                TRUE          1       national
#> 2               FALSE          1       national
#> 3               FALSE          1       national
#> 4               FALSE          1       national
#> 5                TRUE          1       national
#> 6                TRUE          1       national

# Load GDP
df <- pip_load_aux("gdp")
#> Most recent version of data loaded
head(df)
#>   country_code year gdp_data_level      gdp gdp_domain
#> 1          ABW 1986              2 15669.62          1
#> 2          ABW 1987              2 18427.61          1
#> 3          ABW 1988              2 22134.02          1
#> 4          ABW 1989              2 24837.95          1
#> 5          ABW 1990              2 25357.79          1
#> 6          ABW 1991              2 26329.31          1

measure <- "cpi"

# see versions available
df      <- pip_load_aux(measure, version = "available")
#> Versions available for cpi
#>  [1] "2020-09-25 23:30:28 EDT" "2020-09-23 07:37:31 EDT"
#>  [3] "2020-09-22 21:30:54 EDT" "2020-09-22 21:14:13 EDT"
#>  [5] "2020-09-08 11:44:31 EDT" "2020-09-08 11:42:41 EDT"
#>  [7] "2020-09-08 11:41:11 EDT" "2020-08-24 14:25:08 EDT"
#>  [9] "2020-08-24 14:22:42 EDT" "2020-08-24 10:14:32 EDT"
#> [11] "2020-08-11 10:24:42 EDT" "2020-08-11 10:17:47 EDT"
#> [13] "2020-08-11 10:09:44 EDT" "2020-08-07 10:15:48 EDT"
#> [15] "2020-08-06 14:22:36 EDT"
df
#>  [1] "20200925233028" "20200923073731" "20200922213054" "20200922211413"
#>  [5] "20200908114431" "20200908114241" "20200908114111" "20200824142508"
#>  [9] "20200824142242" "20200824101432" "20200811102442" "20200811101747"
#> [13] "20200811100944" "20200807101548" "20200806142236"

# Load version of "2020-08-07 10:15:48 EDT"
df      <- pip_load_aux(measure, version = "20200807101548")
#> Version of data loaded: 2020-08-07 10:15:48
head(df)
#>   country_code surveyid_year reference_year    cpi2011 ccf datalevel
#> 1          AGO          2000        2000.21 0.03384806   1         2
#> 2          AGO          2008        2008.50 0.72333720   1         2
#> 3          AGO          2018        2018.17 3.06059498   1         2
#> 4          ALB          1996        1996.00 0.44443273   1         2
#> 5          ALB          2002        2002.00 0.78028772   1         2
#> 6          ALB          2005        2005.00 0.83847346   1         2

# Load one version before current one (i.e., load previous version)
df      <- pip_load_aux(measure, version = -1)
#> Version of data loaded: 2020-09-23 07:37:31
head(df)
#>   country_code surveyid_year reference_year        cpi ccf cpi_domain
#> 1          AGO          2000        2000.21 0.03384806   1          1
#> 2          AGO          2008        2008.50 0.72333720   1          1
#> 3          AGO          2018        2018.17 3.06059498   1          1
#> 4          ALB          1996        1996.00 0.44443273   1          1
#> 5          ALB          2002        2002.00 0.78028772   1          1
#> 6          ALB          2005        2005.00 0.83847346   1          1
#>   cpi_data_level survey_acronym
#> 1       national            HBS
#> 2       national      IBEP-MICS
#> 3       national          IDREA
#> 4       national            EWS
#> 5       national           LSMS
#> 6       national           LSMS
```

### Inventory of microdata

Check if inventory is up to data and udpate

``` r
# Update inventory of PRY
pip_inventory("update", country = "PRY")
#> data inventory changed for PRY.
#> Data signature has changed, it was not found, or update was forced.
#>  `inventory.fst` has been updated

# Load inventory
df <- pip_inventory()
df$filename[1:5]
#> [1] "AFG_2007_NRVA_V01_M_V05_A_PIP_PC-GPWG.dta"
#> [2] "AFG_2007_NRVA_V01_M_V05_A_PIP_TB-ALL.dta" 
#> [3] "AFG_2011_NRVA_V01_M_V05_A_PIP_PC-GPWG.dta"
#> [4] "AFG_2011_NRVA_V01_M_V05_A_PIP_TB-ALL.dta" 
#> [5] "AGO_2000_HBS_V01_M_V01_A_PIP_PC-GPWG.dta"
```
