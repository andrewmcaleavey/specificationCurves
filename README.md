# specificationCurves

<!-- badges: start -->
<!-- badges: end -->

The goal of specificationCurves is to provide functions for flexible applications of specification curves to diverse data.

## Installation

You can install the development version of specificationCurves from [GitHub](https://github.com) with:

``` r
# requires the {devtools} package to be installed already
library(devtools)
install_github("andrewmcaleavey/specificationCurves")
```

## Example

The most common application at present is through `run_multiple_trims()`:

``` r
library(specificationCurves)
## 
run_multiple_trims(sim_dat)
```

