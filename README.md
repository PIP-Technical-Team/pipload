
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pipload

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/PIP-Technical-Team/pipload/branch/master/graph/badge.svg)](https://app.codecov.io/gh/PIP-Technical-Team/pipload?branch=master)
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
#> Warning: `pip_find_data()` was deprecated in pipload 0.1.13.
#> Please use `pip_find_dlw()` instead.

df$filename
#> [1] "PRY_2017_EPH_V01_M_V02_A_PIP_PC-GPWG.dta"
#> [2] "PRY_2017_EPH_V01_M_V03_A_PIP_PC-GPWG.dta"
#> [3] "PRY_2018_EPH_V01_M_V03_A_PIP_PC-GPWG.dta"
#> [4] "PRY_2018_EPH_V01_M_V04_A_PIP_PC-GPWG.dta"
#> [5] "PRY_2018_EPH_V01_M_V05_A_PIP_PC-GPWG.dta"

# load the data
df2 <- pip_find_data(
             country = "PRY",
             year    = c(2017, 2018),
             tool    = "PC"
)
#> Warning: `pip_find_data()` was deprecated in pipload 0.1.13.
#> Please use `pip_find_dlw()` instead.

names(df2)
#>  [1] "orig"           "filename"       "country_code"   "surveyid_year" 
#>  [5] "survey_acronym" "vermast"        "veralt"         "collection"    
#>  [9] "module"         "tool"           "source"
```

### load Auxiliary data

Load different types of auxiliary data

``` r
# Load CPI
df <- pip_load_aux("cpi")
#> v Most recent version of data loaded:
#> ''//w1wbgencifs01/pip/PIP-Data_QA/_aux/cpi/cpi'.'fst''
head(df)
#>    country_code cpi_year survey_year         cpi ccf survey_acronym
#> 1:          AGO     2000     2000.21 0.033848061   1            HBS
#> 2:          AGO     2008     2008.50 0.723337197   1      IBEP-MICS
#> 3:          AGO     2018     2018.17 2.934414036   1          IDREA
#> 4:          ALB     1996     1996.00 0.444572480   1            EWS
#> 5:          ALB     2002     2002.00 0.780533048   1           LSMS
#> 6:          ALB     2005     2005.00 0.838737128   1           LSMS
#>    change_cpi2011     cpi2011 cpi_domain cpi_domain_value cpi2011_unadj
#> 1:              0 0.033848061          1                1   0.033848061
#> 2:              1 0.723337197          1                1   0.723337197
#> 3:              1 2.934414036          1                1   2.934414036
#> 4:              1 0.444572480          1                1   0.444572480
#> 5:              1 0.780533048          1                1   0.780533048
#> 6:              1 0.838737128          1                1   0.838737128
#>    cpi_final_2019 cpi_data_level cpi2011_SM21 cpi2011_unadj_SM21 cpi2005_SM21
#> 1:             NA       national   0.03385145         0.03385145  0.071889997
#> 2:             NA       national   0.72328920         0.72328920  1.528669953
#> 3:             NA       national   2.93543023         2.93543023           NA
#> 4:             NA       national   0.44446184         0.44446184  0.530049980
#> 5:             NA       national   0.78033877         0.78033877  0.950504005
#> 6:             NA       national   0.83852839         0.83852839  1.000000000
#>        cpi2017          cpi_id
#> 1: 0.014199691 CPI_v06_M_v01_A
#> 2: 0.303449123 CPI_v06_M_v01_A
#> 3: 1.231023884 CPI_v06_M_v01_A
#> 4: 0.399635267 CPI_v06_M_v01_A
#> 5: 0.701637071 CPI_v06_M_v01_A
#> 6: 0.753957905 CPI_v06_M_v01_A

# load PPP
df <- pip_load_aux("ppp")
#> v Most recent version of data loaded:
#> ''//w1wbgencifs01/pip/PIP-Data_QA/_aux/ppp/ppp'.'fst''
head(df)
#>    country_code ppp_year release_version adaptation_version       ppp
#> 1:          ABW     2005              v1                 v1        NA
#> 2:          ABW     2011              v1                 v1 1.6527510
#> 3:          ABW     2011              v1                 v2 1.6527513
#> 4:          ABW     2011              v2                 v1 1.6377631
#> 5:          ABW     2011              v2                 v2 1.6377631
#> 6:          ABW     2017              v1                 v1 1.4804807
#>    ppp_default ppp_default_by_year ppp_domain ppp_data_level
#> 1:       FALSE                TRUE          1       national
#> 2:       FALSE               FALSE          1       national
#> 3:       FALSE               FALSE          1       national
#> 4:       FALSE               FALSE          1       national
#> 5:        TRUE                TRUE          1       national
#> 6:       FALSE                TRUE          1       national

# Load GDP
df <- pip_load_aux("gdp")
#> v Most recent version of data loaded:
#> ''//w1wbgencifs01/pip/PIP-Data_QA/_aux/gdp/gdp'.'fst''
head(df)
#>    country_code year       gdp gdp_data_level gdp_domain
#> 1:          ABW 1986 17231.380       national   national
#> 2:          ABW 1987 20262.945       national   national
#> 3:          ABW 1988 24343.255       national   national
#> 4:          ABW 1989 27313.495       national   national
#> 5:          ABW 1990 27884.253       national   national
#> 6:          ABW 1991 28953.525       national   national

measure <- "cpi"

# see versions available
df      <- pip_load_aux(measure, version = "available")
#> Versions available for cpi
#>  [1] "2022-02-25 14:44:35 EST" "2022-02-23 15:44:16 EST"
#>  [3] "2022-01-25 18:08:08 EST" "2022-01-07 14:38:08 EST"
#>  [5] "2021-04-16 13:04:48 EDT" "2021-03-29 16:34:08 EDT"
#>  [7] "2021-03-03 07:26:44 EST" "2021-02-24 09:44:48 EST"
#>  [9] "2021-02-02 06:46:39 EST" "2021-02-01 13:09:02 EST"
#> [11] "2021-01-29 09:23:20 EST" "2021-01-22 06:47:38 EST"
#> [13] "2020-12-23 13:00:06 EST"
df
#>  [1] "20220225144435" "20220223154416" "20220125180808" "20220107143808"
#>  [5] "20210416130448" "20210329163408" "20210303072644" "20210224094448"
#>  [9] "20210202064639" "20210201130902" "20210129092320" "20210122064738"
#> [13] "20201223130006"

# Load version of "2020-08-07 10:15:48 EDT"
df      <- pip_load_aux(measure, version = "20220223154416")
#> v Version of data loaded: 2022-02-23 15:44:16:
#> '//w1wbgencifs01/pip/PIP-Data_QA/_aux/cpi/_vintage/cpi_20220223154416.fst'
#> i Labels not applied to versioning data
head(df)
#>    country_code cpi_year survey_year         cpi ccf survey_acronym
#> 1:          AGO     2000     2000.21 0.033848061   1            HBS
#> 2:          AGO     2008     2008.50 0.723337197   1      IBEP-MICS
#> 3:          AGO     2018     2018.17 2.934414036   1          IDREA
#> 4:          ALB     1996     1996.00 0.444572480   1            EWS
#> 5:          ALB     2002     2002.00 0.780533048   1           LSMS
#> 6:          ALB     2005     2005.00 0.838737128   1           LSMS
#>    change_cpi2011     cpi2011 cpi_domain cpi_domain_value cpi2011_unadj
#> 1:              0 0.033848061          1                1   0.033848061
#> 2:              1 0.723337197          1                1   0.723337197
#> 3:              1 2.934414036          1                1   2.934414036
#> 4:              1 0.444572480          1                1   0.444572480
#> 5:              1 0.780533048          1                1   0.780533048
#> 6:              1 0.838737128          1                1   0.838737128
#>    cpi_final_2019 cpi_data_level cpi2011_SM21 cpi2011_unadj_SM21 cpi2005_SM21
#> 1:             NA       national   0.03385145         0.03385145  0.071889997
#> 2:             NA       national   0.72328920         0.72328920  1.528669953
#> 3:             NA       national   2.93543023         2.93543023           NA
#> 4:             NA       national   0.44446184         0.44446184  0.530049980
#> 5:             NA       national   0.78033877         0.78033877  0.950504005
#> 6:             NA       national   0.83852839         0.83852839  1.000000000
#>        cpi2017          cpi_id
#> 1: 0.014199691 CPI_v06_M_v01_A
#> 2: 0.303449123 CPI_v06_M_v01_A
#> 3: 1.231023884 CPI_v06_M_v01_A
#> 4: 0.399635267 CPI_v06_M_v01_A
#> 5: 0.701637071 CPI_v06_M_v01_A
#> 6: 0.753957905 CPI_v06_M_v01_A

# Load one version before current one (i.e., load previous version)
df      <- pip_load_aux(measure, version = -1)
#> v Version of data loaded: 2022-02-23 15:44:16:
#> '//w1wbgencifs01/pip/PIP-Data_QA/_aux/cpi/_vintage/cpi_20220223154416.fst'
#> i Labels not applied to versioning data
head(df)
#>    country_code cpi_year survey_year         cpi ccf survey_acronym
#> 1:          AGO     2000     2000.21 0.033848061   1            HBS
#> 2:          AGO     2008     2008.50 0.723337197   1      IBEP-MICS
#> 3:          AGO     2018     2018.17 2.934414036   1          IDREA
#> 4:          ALB     1996     1996.00 0.444572480   1            EWS
#> 5:          ALB     2002     2002.00 0.780533048   1           LSMS
#> 6:          ALB     2005     2005.00 0.838737128   1           LSMS
#>    change_cpi2011     cpi2011 cpi_domain cpi_domain_value cpi2011_unadj
#> 1:              0 0.033848061          1                1   0.033848061
#> 2:              1 0.723337197          1                1   0.723337197
#> 3:              1 2.934414036          1                1   2.934414036
#> 4:              1 0.444572480          1                1   0.444572480
#> 5:              1 0.780533048          1                1   0.780533048
#> 6:              1 0.838737128          1                1   0.838737128
#>    cpi_final_2019 cpi_data_level cpi2011_SM21 cpi2011_unadj_SM21 cpi2005_SM21
#> 1:             NA       national   0.03385145         0.03385145  0.071889997
#> 2:             NA       national   0.72328920         0.72328920  1.528669953
#> 3:             NA       national   2.93543023         2.93543023           NA
#> 4:             NA       national   0.44446184         0.44446184  0.530049980
#> 5:             NA       national   0.78033877         0.78033877  0.950504005
#> 6:             NA       national   0.83852839         0.83852839  1.000000000
#>        cpi2017          cpi_id
#> 1: 0.014199691 CPI_v06_M_v01_A
#> 2: 0.303449123 CPI_v06_M_v01_A
#> 3: 1.231023884 CPI_v06_M_v01_A
#> 4: 0.399635267 CPI_v06_M_v01_A
#> 5: 0.701637071 CPI_v06_M_v01_A
#> 6: 0.753957905 CPI_v06_M_v01_A
```

### Inventory of microdata

Check if inventory is up to data and udpate

``` r
# Update inventory of PRY
pip_inventory("update", country = "PRY")
#> i reading PIP directory
#> v reading PIP directory [3.2s]
#> 
#> i file 'inventory.fst' is up to date.
#> No update performed

# Load inventory
df <- pip_inventory()
df$filename[1:5]
#> [1] "AGO_2000_HBS_V01_M_V01_A_PIP_PC-GPWG.dta"      
#> [2] "AGO_2008_IBEP-MICS_V02_M_V02_A_PIP_PC-GPWG.dta"
#> [3] "AGO_2008_IBEP-MICS_V02_M_V02_A_PIP_TB-ALL.dta" 
#> [4] "AGO_2018_IDREA_V01_M_V01_A_PIP_PC-GPWG.dta"    
#> [5] "AGO_2018_IDREA_V01_M_V01_A_PIP_TB-ALL.dta"
```
