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

## Process the wq limits (guidelines) in limits.csv for inclusion in the
## package. See limits.Rmd for details

library(wqbc)
library(dplyr)
library(magrittr)
library(devtools)

rm(list = ls())

input_limits <- function() {
  limits <- read.csv("data-raw/limits.csv", na.strings = c("NA", ""), stringsAsFactors = FALSE)

  stopifnot(identical(
    colnames(limits),
    c(
      "Variable", "Term",
      "Condition", "UpperLimit", "Units", "Table",
      "Reference", "Use"
    )
  ))

  stopifnot(all(!is.na(select(limits, -Condition))))

  stopifnot(all(limits$Term %in% c("Short", "Long")))
  stopifnot(all(limits$Units %in% lookup_units()))
  stopifnot(all(limits$Reference %in% c(
    "BC_2006", "BC_2015", "MOE_PERS_COMM_2014",
    "MOE_PERS_COMM_2015", "CANADA_2014"
  )))
  stopifnot(all(limits$Use %in% c("Freshwater Life")))

  check_valid_expression <- function(x) {
    parse(text = x)
    TRUE
  }

  check_valid_expression(limits$Condition)
  check_valid_expression(limits$UpperLimit)

  limits <- rename_(limits, "..Units" = "Units")

  load("data/codes.rda")

  stopifnot(all(limits$Variable %in% codes$Variable))

  limits <- inner_join(limits, codes, by = "Variable")

  stopifnot(all(limits$..Units == limits$Units))
  limits$..Units <- NULL

  limits %<>% arrange(Variable, Use, Term)

  limits %<>% select(Variable, Use, Term, Condition, UpperLimit, Units)

  limits
}
limits <- input_limits()
use_data(limits, overwrite = TRUE, compress = "xz")
