
<!-- README.md is generated from README.Rmd. Please edit that file -->
    ## 
    ## Attaching package: 'dplyr'

    ## Die folgenden Objekte sind maskiert von 'package:stats':
    ## 
    ##     filter, lag

    ## Die folgenden Objekte sind maskiert von 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

TregaTa
=======

Overview
--------

tregata will be a (meta-)package for waterchemistry and gasflux calculations. It is initially designed for the TregaTa Project and may include some project related functions at early stages.

Installation
------------

``` r
# The package is not yet released on CRAN.
# Development version from GitHub:
# install.packages("devtools")
devtools::install_github("tekknosol/tregata")
```

Usage
-----

``` r
data("gasmet", "meta")
flux_raw <- preprocess_gasmet(gasmet, meta)
glimpse(flux_raw)
#> Observations: 150
#> Variables: 10
#> $ spot          <chr> "BZT", "BZT", "BZT", "BZT", "BZT", "BZT", "BZT",...
#> $ day           <chr> "08.05.2018", "08.05.2018", "08.05.2018", "08.05...
#> $ rep           <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1", "1"...
#> $ V             <dbl> 0.01461, 0.01461, 0.01461, 0.01461, 0.01461, 0.0...
#> $ A             <dbl> 0.098, 0.098, 0.098, 0.098, 0.098, 0.098, 0.098,...
#> $ Time          <dbl> 0.000000000, 0.008333333, 0.016666667, 0.0250000...
#> $ Concentration <dbl> 16.83826, 16.56487, 16.39089, 16.22935, 16.11750...
#> $ CH4mmol       <dbl> 0.09030110, 0.08657308, 0.09154378, 0.08988688, ...
#> $ co2           <chr> "co2", "co2", "co2", "co2", "co2", "co2", "co2",...
#> $ ch4           <chr> "ch4", "ch4", "ch4", "ch4", "ch4", "ch4", "ch4",...
```
