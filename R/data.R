#' CCME Water Quality Index User's Manual Example Data
#'
#' A tidy data.frame of the CCME Water Quality Index 1.0 User's Manual
#' example dataset.
#'
#' @format A data frame with 120 rows and 6 columns:
#' \describe{
#'   \item{Date}{The date of the reading.}
#'   \item{Variable}{The name of the variable.}
#'   \item{Value}{The value of the reading.}
#'   \item{DetectionLimit}{The detection limit.}
#'   \item{LowerLimit}{The maximum permitted value.}
#'   \item{UpperLimit}{The maximum permitted value.}
#'   \item{Units}{The units.}
#' }
#' @examples
#' demo(ccme)
"ccme"

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

#' Dummy Water Quality Data
#'
#' A dummy data set to illustrate various data cleaning functions.
#'
#' @format A data frame with 5 columns:
#' \describe{
#'   \item{Date}{The date of the reading.}
#'   \item{Variable}{The name of the variable.}
#'   \item{Value}{The value of the reading.}
#'   \item{Units}{The value's units.}
#' }
#' @examples
#' demo(dummy)
"dummy"

#' Fraser River Basin Long-term Water Quality Monitoring 1979-Present
#'
#' Surface freshwater quality monitoring in the Fraser River Basin
#' is carried out under the Canada-British Columbia Water Quality
#' Monitoring Agreement. Monitoring is conducted to assess water
#' quality status and long-term trends, detect emerging issues,
#' establish water quality guidelines and track the effectiveness
#' of remedial measures and regulatory decisions.
#'
#' @details The original dataset has been filtered to remove values
#' for variables without currently defined limits.
#' In addition, unimportant columns have been dropped and the remaining
#' columns renamed.
#'
#' @format A data frame with 9 columns:
#' \describe{
#'   \item{SiteID}{The unique water quality station number.}
#'   \item{Date}{The date of the reading.}
#'   \item{Variable}{The name of the variable.}
#'   \item{Value}{The value of the reading.}
#'   \item{Units}{The value's units.}
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

