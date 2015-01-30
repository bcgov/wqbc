#' CCME Water Quality Index User's Manual Example Data
#'
#' A tidy data.frame of the CCME Water Quality Index 1.0 User's Manual
#' example dataset.
#'
#' @format A data frame with 103 rows and 6 columns:
#' \describe{
#'   \item{Date}{The date of the reading.}
#'   \item{Variable}{The name of the variable.}
#'   \item{Value}{The value of the reading.}
#'   \item{LowerLimit}{The minimum permitted value.}
#'   \item{UpperLimit}{The maximum permitted value.}
#' }
#' @examples
#' demo(ccme, ask = FALSE)
"ccme"

#' Dummy Water Quality Data
#'
#' A dummy data set to illustrate various data cleaning functions.
#'
#' @format A data frame with 5 columns:
#' \describe{
#'   \item{ID}{A row identifier.}
#'   \item{Date}{The date of the reading.}
#'   \item{Variable}{The name of the variable.}
#'   \item{Value}{The value of the reading.}
#'   \item{Units}{The value's units.}
#' }
#' @examples
#' demo(dummy, ask = FALSE)
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
#' demo(fraser, ask = FALSE)
#' }
"fraser"
