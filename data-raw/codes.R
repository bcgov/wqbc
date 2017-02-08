# Copyright 2015 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

## Process codes.csv (containing parameter codes, units, and averaging method)
## for use in the package

library(wqbc)
library(dplyr)
library(magrittr)
library(devtools)

rm(list = ls())

input_codes <- function() {

  codes <- read.csv("data-raw/codes.csv", na.strings = c("NA", ""), stringsAsFactors = FALSE)
  stopifnot(identical(colnames(codes), c("Variable","Code","Units", "Average", "EC_Code")))

  stopifnot(all(!is.na(codes[c("Variable","Code","Units", "Average")])))

  stopifnot(!anyDuplicated(codes$Code))
  stopifnot(!anyDuplicated(codes$Variable))
  stopifnot(all(codes$Units %in% lookup_units()))
  stopifnot(all(codes$Average %in% c("mean", "median")))


  codes
}
codes <- input_codes()
devtools::use_data(codes, overwrite = TRUE, compress = "xz")
