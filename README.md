
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wqbc

<!-- badges: start -->
[![Lifecycle:Dormant](https://img.shields.io/badge/Lifecycle-Dormant-ff7f2a)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)
[![img](https://img.shields.io/badge/Lifecycle-Maturing-007EC6)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)
[![R build
status](https://github.com/bcgov/wqbc/workflows/R-CMD-check/badge.svg)](https://github.com/bcgov/wqbc/actions)
[![Codecov test
coverage](https://codecov.io/gh/bcgov/wqbc/branch/master/graph/badge.svg)](https://codecov.io/gh/bcgov/wqbc?branch=master)
<!-- badges: end -->

## Overview

The `wqbc` R package facilitates cleaning and tidying water quality data
and calculating water quality thresholds for British Columbia.
Previously it also calculated the [CCME Water Quality
Index](http://www.ccme.ca/en/resources/canadian_environmental_quality_guidelines/index.html)
but that functionality has been moved to the
[wqindex](https://github.com/bcgov/wqindex) package.

`wqbc` was written by B.C. Ministry of Environment and [Poisson
Consulting](http://www.poissonconsulting.ca/) team members.

## Usage

For more information please see the
[vignette](https://htmlpreview.github.com/?https://github.com/bcgov/wqbc/master/inst/doc/wqbc.html).
In your R session, you can type `vignette("wqbc")` to see the vignette.
Please note that this vignette is currently out of date as it includes
information on calculating the Water Quality Index (which has been moved
to its own package [wqindex](https://github.com/bcgov/wqindex)).

## Install

To install and load the latest version of wqbc:

``` r
# install.packages("remotes") # if remotes is not installed
remotes::install_github("bcgov/wqbc")
library(wqbc)
```

## Project Status

This package is under development. The user is responsible for checking
all variables and limits that they use.

## Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an
[issue](https://github.com/bcgov/wqbc/issues/).

## How to Contribute

If you would like to contribute to the package, please see our
[CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

## License

    Copyright 2015 Province of British Columbia
    
    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at 
    
       http://www.apache.org/licenses/LICENSE-2.0
    
    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.

This repository is maintained by [Environmental Reporting
BC](http://www2.gov.bc.ca/gov/content?id=FF80E0B985F245CEA62808414D78C41B).
Click [here](https://github.com/bcgov/EnvReportBC-RepoList) for a
complete list of our repositories on GitHub.
