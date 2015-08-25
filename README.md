<!-- README.md is generated from README.Rmd. Please edit that file -->
<a rel="Exploration" href="https://github.com/BCDevExchange/docs/blob/master/discussion/projectstates.md"><img alt="Being designed and built, but in the lab. May change, disappear, or be buggy." style="border-width:0" src="http://bcdevexchange.org/badge/2.svg" title="Being designed and built, but in the lab. May change, disappear, or be buggy." /></a>

Water Quality Thresholds and Index Calculations for British Columbia
====================================================================

The `wqbc` R package calculates water quality thresholds and water quality indices and plots water quality indices spatially and temporally for British Columbia.

This package was written for the B.C. Ministry of Environment by [Poisson Consulting](http://www.poissonconsulting.ca/).

Useage
------

For information on use, the reader is referred to vignette("wqbc").

Install
-------

To install the latest version of wqbc:

    # install.packages("devtools")
    library(devtools)
    install_github("bcgov/wqbc", build_vignettes = TRUE)
    # Using `build_vignettes = TRUE` will slow down the install, but is necessary if 
    # you want to read the vignette, which is recommended
    library(wqbc)

Project Status
--------------

This package is still under development.

Getting Help or Reporting an Issue
----------------------------------

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov/wqbc/issues/).

How to Contribute
-----------------

If you would like to contribute to the package, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

License
-------

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
