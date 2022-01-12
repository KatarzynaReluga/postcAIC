
<!-- README.md is generated from README.Rmd. Please edit that file -->

# postcAIC

<!-- badges: start -->
<!-- badges: end -->

## Overview

Package **postcAIC** implements post-cAIC confidence intervals for mixed
and fixed parameters under linear mixed models.

Reference: Claeskens, Reluga, and Sperlich (2021). *Post-selection
inference for linear mixed model parameters using the conditional Akaike
information criterion*. Available at <https://arxiv.org/abs/2109.10975>

## Installation

You can install the most recent version of postcAIC from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")

install_version("tmg", version = "0.3", repos = "http://cran.r-project.org")
devtools::install_github("KatarzynaReluga/postcAIC")

library(postcAIC)
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(postcAIC)
## basic example code
```

## Simulation Study

All simulation studies from Claeskens, Reluga, and Sperlich (2021) can
be reproduced using codes in folder
[simulations](https://github.com/KatarzynaReluga/postcAIC/tree/main/simulations).
