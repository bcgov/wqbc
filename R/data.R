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

#' CCME Water Quality Index User's Manual Example Data
#'
#' The Canadian Council of Ministers of the Environment (CCME)
#' Water Quality Index 1.0 User's Manual example dataset in tidy format.
#'
#' @format A data frame with 120 rows and 7 columns:
#' \describe{
#'   \item{Date}{The date of the reading.}
#'   \item{Variable}{The name of the variable.}
#'   \item{Value}{The value of the reading.}
#'   \item{DetectionLimit}{The detection limit.}
#'   \item{LowerLimit}{The minimum permitted value.}
#'   \item{UpperLimit}{The maximum permitted value.}
#'   \item{Units}{The units of the value, detection limit and lower and upper limits.}
#' }
#' @examples
#' demo(ccme)
"ccme"

#' Water Quality Parameter Codes and Units for British Columbia
#'
#' The standard variables and codes recognised by the wqbc package with their
#' standard units and the R function to use when averaging multiple samples.
#'
#' @format A data frame with 4 variables:
#' \describe{
#'   \item{Variable}{The name of the variable.}
#'   \item{Code}{The EMS code in expanded form.}
#'   \item{Units}{The standard units for the variable.}
#'   \item{Average}{R function to calculate "average" value.}
#' }
#' @seealso \code{\link{calc_limits}}
"codes"

#' Dummy Water Quality Data
#'
#' A dummy data set to illustrate various data cleaning functions.
#'
#' @format A data frame with 4 columns:
#' \describe{
#'   \item{Date}{The date of the reading.}
#'   \item{Variable}{The name of the variable.}
#'   \item{Value}{The value of the reading.}
#'   \item{Units}{The units of the value.}
#' }
#' @examples
#' demo(dummy)
#' @seealso \code{\link{clean_wqdata}}
"dummy"

#' Water Quality Parameter EMS Names and Codes for British Columbia
#'
#' The standard variables and codes stored in the EMS database.
#'
#' @format A tibble with 4 variables:
#' \describe{
#'   \item{Variable}{The name of the variable.}
#'   \item{Code}{The EMS code.}
#' }
"ems_codes"

#' Fraser River Basin Long-term Water Quality Monitoring 1979-Present
#'
#' Surface freshwater quality monitoring in the Fraser River Basin
#' is carried out under the Canada-British Columbia Water Quality
#' Monitoring Agreement. Monitoring is conducted to assess water
#' quality status and long-term trends, detect emerging issues,
#' establish water quality guidelines and track the effectiveness
#' of remedial measures and regulatory decisions.
#'
#' @format A data frame with 8 columns:
#' \describe{
#'   \item{SiteID}{The unique water quality station number.}
#'   \item{Date}{The date of the reading.}
#'   \item{Variable}{The name of the variable.}
#'   \item{Value}{The value of the reading.}
#'   \item{Units}{The units of the value.}
#'   \item{Site}{The full name of the station.}
#'   \item{Lat}{The latitude of the station in decimal degrees.}
#'   \item{Long}{The longitude of the station in decimal degrees.}
#' }
#' @source \url{http://open.canada.ca/data/en/dataset/9ec91c92-22f8-4520-8b2c-0f1cce663e18}
#' @examples
#' \dontrun{
#' demo(fraser)
#' }
"fraser"

#' Water Quality Limits for British Columbia
#'
#' The short and long term water quality limits for British Columbia recognised
#' by the wqbc package.
#'
#' @format A data frame with 5 variables:
#' \describe{
#'   \item{Variable}{The name of the variable.}
#'   \item{Term}{The term of the limit i.e. "Short" versus "Long".}
#'   \item{Condition}{A logical R expression to test for the required condition.}
#'   \item{UpperLimit}{The upper limit or an R expression defining the upper limit.}
#'   \item{Units}{The units of the upper limit.}
#' }
#' @seealso \code{\link{calc_limits}}
"limits"

#' Water Quality Stations for British Columbia
#'
#' The water quality stations for British Columbia with their coordinates.
#'
#' @format A tibble with 4 variables:
#' \describe{
#'   \item{EMS_ID}{The station ID (chr).}
#'   \item{Station_Name}{The EMS name of the station (chr).}
#'   \item{Latitude}{The station latitude in decimal degrees (dbl).}
#'   \item{Longitude}{The station longitude in decimal degrees (dbl).}
#' }
"stations"

#' Example data used in Yue, Pilon et al. 2001 taken from the
#'   Canadian National Water Data Archive (HYDAT) 1949-1998
#'
#' Hydrometric data are collected and compiled by Water Survey of
#' Canada’s eight regional offices. The information is housed in
#' two centrally-managed databases: HYDEX and HYDAT.
#'
#' HYDAT is a relational database that contains the actual computed
#' data for the stations listed in HYDEX. These data include: daily and
#' monthly means of flow, water levels and sediment concentrations
#' (for sediment sites). For some sites, peaks and extremes are also
#' recorded.
#'
#' WSC now offers hydrometric data and station information in a single
#' downloadable file, either in Microsoft Access Database format or in
#' SQLite format, updated on a quarterly basis.
#'
#' This database was used to derive the yuepilon dataset, which is a
#' table of annual mean river flows for four sites: 02FB007, 02KB001,
#' 02EA005 and 02GA010.
#'
#' @format A data frame with 5 columns:
#' \describe{
#'   \item{SiteID}{Unique 7-character station identification code.}
#'   \item{Date}{The date of the reading.}
#'   \item{Variable}{The name of the variable.}
#'   \item{Value}{The value of the reading.}
#'   \item{Units}{The units of the value.}
#'   \item{Site}{The full name of the station.}
#'   \item{Lat}{The latitude of the station in decimal degrees.}
#'   \item{Long}{The longitude of the station in decimal degrees.}
#' }
#' @source \url{http://www.ec.gc.ca/rhc-wsc/default.asp?lang=En&n=9018B5EC-1}
#' @examples
#' \dontrun{
#' demo(yuepilon)
#' }
"yuepilon"
