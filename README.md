
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wqbc

<!-- badges: start -->

[![BCDevExchange
Status](https://assets.bcdevexchange.org/images/badges/exploration.svg)](https://github.com/BCDevExchange/docs/blob/master/discussion/projectstates.md)
[![Travis-CI Build
Status](https://travis-ci.org/bcgov/wqbc.svg?branch=master)](https://travis-ci.org/bcgov/wqbc)
[![Codecov test
coverage](https://codecov.io/gh/bcgov/wqbc/branch/master/graph/badge.svg)](https://codecov.io/gh/bcgov/wqbc?branch=master)
<!-- badges: end -->

## Overview

The `wqbc` R package helps clean and process water quality data and
calculates water quality thresholds for British Columbia. Previously
this package also calculated the [CCME Water Quality
Index](http://www.ccme.ca/en/resources/canadian_environmental_quality_guidelines/index.html)
but that functionality has been moved to its own package:
[wqindex](https://github.com/bcgov/wqindex).

This package was written for the B.C. Ministry of Environment by
[Poisson Consulting](http://www.poissonconsulting.ca/). Ministry of
Environment staff maintain the package.

## Usage

For information on use, please see the
[vignette](https://htmlpreview.github.com/?https://github.com/bcgov/wqbc/master/inst/doc/wqbc.html).
In your R session, you can type `vignette("wqbc")` to see the vignette.
Please note that this vignette is currently out of date as it includes
information on calculating the Water Quality Index (which has been moved
to its own package [wqindex](https://github.com/bcgov/wqindex)).

## Install

To install the latest version of wqbc:

``` r
# install the devtools package if it's not already installed
# install.packages("devtools")
library(devtools)
install_github("bcgov/wqbc")
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
