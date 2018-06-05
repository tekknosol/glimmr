
<!-- README.md is generated from README.Rmd. Please edit that file -->
TregaTa
=======

Overview
--------

tregata will be a (meta-)package covering waterchemistry, waterphysics and gasflux calculations. It was initially designed for the TregaTa Project and may include some project related functions at early stages.

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

First check preprocessed data:

``` r
data("gasmet", "meta")
flux_raw <- process_gasmet(gasmet, meta, pre=T)
glimpse(flux_raw)
#> Observations: 150
#> Variables: 12
#> $ spot    <chr> "BZT", "BZT", "BZT", "BZT", "BZT", "BZT", "BZT", "BZT"...
#> $ day     <chr> "08.05.2018", "08.05.2018", "08.05.2018", "08.05.2018"...
#> $ rep     <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "2",...
#> $ V       <dbl> 0.01461, 0.01461, 0.01461, 0.01461, 0.01461, 0.01461, ...
#> $ A       <dbl> 0.098, 0.098, 0.098, 0.098, 0.098, 0.098, 0.098, 0.098...
#> $ Time    <dbl> 0.000000000, 0.008333333, 0.016666667, 0.025000000, 0....
#> $ CO2mmol <dbl> 16.82954, 16.55629, 16.38240, 16.22094, 16.10916, 15.9...
#> $ CH4mmol <dbl> 0.09025434, 0.08652824, 0.09149637, 0.08984033, 0.0964...
#> $ N2Ommol <dbl> 0.009232439, 0.008735627, 0.008570022, 0.008652824, 0....
#> $ co2     <chr> "co2", "co2", "co2", "co2", "co2", "co2", "co2", "co2"...
#> $ ch4     <chr> "ch4", "ch4", "ch4", "ch4", "ch4", "ch4", "ch4", "ch4"...
#> $ n2o     <chr> "n2o", "n2o", "n2o", "n2o", "n2o", "n2o", "n2o", "n2o"...
```

Compute fluxes with:

``` r
flux <- process_gasmet(gasmet, meta)
```
