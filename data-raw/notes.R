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


#' Water Quality Parameter codes and units
#'
#' @details Water Quality Parameter codes and units
#'
#' @format A data frame with 3 variables:
#' \describe{
#'   \item{Code}{unique short-hand code}
#'   \item{Variable}{unique name of water quality parameter}
#'   \item{Units}{units for parameter}
#'   \item{Average}{R function to calculate "average" value for multiple samples in a period}
#' }
"codes"

#' Water Quality Limits for British Columbia and Canada
#'
#' @format A data frame with 13 variables:
#' \describe{
#'   \item{Variable}{name of water quality parameter}
#'   \item{Term}{Period for which limit applies. Either Short which
#'   indicates any individual measurement or Long which indicates
#'   average of at least 5 values within a 30 day period.}
#'   \item{Condition}{R logical expression to test required condition}
#'   \item{UpperLimit}{R expression defining upper limit}
#'   \item{Units}{units for limit}
#' }
"limits"
