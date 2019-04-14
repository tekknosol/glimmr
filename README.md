
<!-- README.md is generated from README.Rmd. Please edit that file -->
glimmr
======

Gas fLuxes and dynamIc chaMber MeasuRements

[![tidyverse\_lifecycle\_badge](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) [![Travis-CI Build Status](https://travis-ci.org/tekknosol/glimmr.svg?branch=master)](https://travis-ci.org/tekknosol/glimmr)

Overview
--------

The `glimmr` package provides functions to convert high frequency concentration data obtained by (dynamic-) chamber measurements into gasfluxes. Data recorded with different devices can be processed automatically. The data is separated into chunks covering the single chamber applications and containing additional meta information (e.g. IDs, temperature...) provided by a metadata file.

Furthermore `glimmr` contains functions to calculate gasfluxes with the 'Boundary Layer Equation' (BLE).

To calculate gasfluxes from high frequency chamber measurements, glimmr offers a cusomizable system of gas analyzer definitions (`analyzer()`), as well as two preconfigured devices with `read_`, `process_` and `inspect_` functions:

-   `process_chamber()` & `inspect_chamber()`: custom devices
-   `read_gasmet()`, `process_gasmet()` & `inspect_gasmet()`: GASMET
-   `read_losgatos()`, `process_losgatos()` & `inspect_losgatos()`: LosGatos

Installation
------------

``` r
# The package is not released on CRAN yet.
# Development version from GitHub:
# install.packages("devtools")
devtools::install_github("tekknosol/glimmr")
```

Getting started
---------------

Calculate fluxes from device records and a meta file containing information about single measurements. Fluxes are reported in \[mmol/m²/d\] using linear model fitting and robust linear model fitting with `robust::lmRob()`

``` r
# read data
## gasmet <- read_gasmet("path/to/gasmet_file.txt")
## meta <- read_csv("path/to/meta_file.csv")

# compute fluxes in mmol/m²/d
# F_LM: Liner Model
# F_RLM: Robust Liner Model
process_gasmet(gasmet, meta_gasmet)
#> End of interval determined by number of observations. Count = 10
#> Using temperature from meta file. Column = temp
#> Warning in test.lmRob(object): Denominator smaller than tl= 1e-06 in test
#> for bias.
#> # A tibble: 18 x 9
#>    date       site  begin               gas     rep     F_LM   LM_r2
#>    <date>     <chr> <dttm>              <chr> <dbl>    <dbl>   <dbl>
#>  1 2017-04-12 YT1   2017-04-12 09:01:49 CH4       1  2.59e-2 0.0119 
#>  2 2017-04-12 YT1   2017-04-12 09:08:56 CH4       2 -1.44e-1 0.360  
#>  3 2017-04-12 YT1   2017-04-12 09:14:25 CH4       3  9.80e-2 0.118  
#>  4 2017-04-12 YTMe… 2017-04-12 16:47:41 CH4       1  1.51e-2 0.00160
#>  5 2017-04-12 YTMe… 2017-04-12 16:55:41 CH4       2 -6.01e-2 0.0449 
#>  6 2017-04-12 YTMe… 2017-04-12 17:02:20 CH4       3  1.07e-1 0.307  
#>  7 2017-04-12 YT1   2017-04-12 09:01:49 CO2       1 -7.58e+0 0.533  
#>  8 2017-04-12 YT1   2017-04-12 09:08:56 CO2       2  1.73e+0 0.0987 
#>  9 2017-04-12 YT1   2017-04-12 09:14:25 CO2       3 -9.45e-1 0.101  
#> 10 2017-04-12 YTMe… 2017-04-12 16:47:41 CO2       1  2.59e+1 0.987  
#> 11 2017-04-12 YTMe… 2017-04-12 16:55:41 CO2       2  2.31e+1 0.953  
#> 12 2017-04-12 YTMe… 2017-04-12 17:02:20 CO2       3  2.66e+1 0.991  
#> 13 2017-04-12 YT1   2017-04-12 09:01:49 N2O       1 -4.19e-2 0.282  
#> 14 2017-04-12 YT1   2017-04-12 09:08:56 N2O       2  2.62e-2 0.279  
#> 15 2017-04-12 YT1   2017-04-12 09:14:25 N2O       3  1.11e-2 0.153  
#> 16 2017-04-12 YTMe… 2017-04-12 16:47:41 N2O       1  4.09e-3 0.0134 
#> 17 2017-04-12 YTMe… 2017-04-12 16:55:41 N2O       2  1.54e-2 0.176  
#> 18 2017-04-12 YTMe… 2017-04-12 17:02:20 N2O       3  1.21e-2 0.0712 
#> # … with 2 more variables: F_RLM <dbl>, RLM_r2 <dbl>
```

Calculate gas transfer coefficient used with the Boundary Layer Equation (BLE).

``` r
# Windpseed in m/s
windspeed <- c(2, 4.3, 1.8) 
# Water temperature in °C
watertemperature <- c(18, 21, 19.3) 

calc_kW(windspeed, watertemperature)
#> [1] 1.488888 7.426978 1.246858
```
