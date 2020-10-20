
# vietnamcovid19
<!-- badges: start -->
[![R build status](https://github.com/etc5523-2020/r-package-assessment-HanseNgo305/workflows/R-CMD-check/badge.svg)](https://github.com/etc5523-2020/r-package-assessment-HanseNgo305/actions)
<!-- badges: end -->

The goal of _vietnamcovid19_ is to provide a simple dashboard to report the situation of Covid-19 in Vietnam from the January 2020 to October 2020.

## Installation

You can install the  _vietnamcovid19_ from **Github** using:  

``` r
# install.packages("devtools")
devtools::install_github("etc5523-2020/r-package-assessment-HanseNgo305")
```

## Example

Some function you can use in this package would include

``` r
library(vietnamcovid19)
library(tibble)

# launch the app
# launch_app()

# Check the dataset for Vietnam cases and deaths by province
vietnam_daily

# Check the dataset for Vietnam patients
patient_node

# Check the list of references used
ref
```

