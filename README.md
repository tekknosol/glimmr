
<!-- README.md is generated from README.Rmd. Please edit that file -->
TregaTa
=======

[![tidyverse\_lifecycle\_badge](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

Overview
--------

tregata is a (meta-)package providing routines for waterchemistry, waterphysics and gasflux calculations. It was initially designed for the TregaTa Project and may include some project related functions at early stages.

To calculate gasfluxes from high frequency chamber measurements, tregata supports two devices with two `read_` and `process_` functions:

-   `read_gasmet()` & `process_gasmet()`: GASMET
-   `read_losgatos()` & `process_losgatos()`: LosGatos

Installation
------------

``` r
# The package is not released on CRAN yet.
# Development version from GitHub:
# install.packages("devtools")
devtools::install_github("tekknosol/tregata")
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
