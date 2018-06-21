
<!-- README.md is generated from README.Rmd. Please edit that file -->
glimmr
======

Gas fLuxes and dynamIc chaMber MeasuRements

[![tidyverse\_lifecycle\_badge](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) [![Travis-CI Build Status](https://travis-ci.org/tekknosol/glimmr.svg?branch=master)](https://travis-ci.org/tekknosol/glimmr)

Overview
--------

The `glimmr` package provides functions to convert high frequency concentration data obtained by (dynamic-) chamber measurements into gasfluxes. Data recorded with different devices can be processed automatically. The data is separated into chunks covering the single chamber applications and containing additional meta information (e.g. IDs, temperature...) provided by a metadata file. The model fitting for the different gas species is done with the `gasfluxes` package.

Furthermore `glimmr` contains functions to calculate gasfluxes with the 'Boundary Layer Equation' (BLE).

To calculate gasfluxes from high frequency chamber measurements, glimmr supports two devices with two `read_`, `process_` and `inspect_` functions:

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

Calculate fluxes from device records and a meta file containing information about single measurements.

``` r
# read data
gasmet <- read_gasmet("path/to/gasmet_file.txt")
meta <- read_csv("path/to/meta_file.csv")

# compute fluxes
flux <- process_gasmet(gasmet, meta)
```

Calculate gas transfer coefficient used with the Boundary Layer Equation (BLE).

``` r
windspeed <- c(2, 4.3, 1.8)
watertemperature <- c(18, 21, 19.3)

calc_kW(windspeed, watertemperature)
#> [1] 1.488888 7.426978 1.246858
```
