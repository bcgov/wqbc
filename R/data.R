#' CCME Water Quality Index User's Manual Example Data
#'
#' A tidy data.frame of the CCME Water Quality Index 1.0 User's Manual
#' example dataset.
#'
#' @format A data frame with 120 rows and 6 variables:
#' \describe{
#'   \item{Code}{parameter code}
#'   \item{Date}{date of reading}
#'   \item{Value}{value of reading}
#'   \item{DetectionLimit}{detection limit of method}
#'   \item{LowerLimit}{minimum permitted value}
#'   \item{UpperLimit}{maximum permitted value}
#' }
"ccme"

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
#' for variables without currently defined limits. In addition,
#' variables are referenced
#' by code, unimportant columns have been dropped and the remaining
#' columns renamed.
#'
#' @format A data frame with 17,837 rows and 9 variables:
#' \describe{
#'   \item{SiteID}{unique water quality station number}
#'   \item{Date}{date of water quality sample}
#'   \item{Code}{parameter code}
#'   \item{Value}{measured value}
#'   \item{Units}{reading units}
#'   \item{DetectionLimit}{minimum value of method}
#'   \item{Site}{full station name}
#'   \item{Latitude}{latitude in decimal degrees}
#'   \item{Longitude}{longitude in decimal degrees}
#' }
#' @source \url{http://open.canada.ca/data/en/dataset/9ec91c92-22f8-4520-8b2c-0f1cce663e18}
"fraser"

#' Water Quality Limits for British Columbia and Canada
#'
#' Both the Canadian federal government and the
#' province of British Columbia set limits for a range of
#' water quality parameters. The data were taken from a
#' range of federal and provincial websites. They represent
#' long-term limits for samples taken from the water column.
#' Limits which depend on conditions from other periods
#' or locations are not included. For example some of the
#' limits for turbidity are defined with respect to background
#' levels and/or an upstream site.
#' Where some interpretation of the information provided was required
#' this is noted in the comments.
#'
#' @format A data frame with 13 variables:
#' \describe{
#'   \item{Code}{short-hand code}
#'   \item{Variable}{name of water quality parameter}
#'   \item{Jurisdiction}{regulatory jurisdiction in Canada (permitted values:
#'   BC, CA)}
#'   \item{Use}{intended use (permitted values: Drinking, Freshwater Life,
#'    Marine Life, Wildlife, Livestock, Irrigation, Recreation)}
#'   \item{SubUse}{additional information on use}
#'   \item{Samples}{minimum number of samples required}
#'   \item{Days}{period within which number of samples must be collected}
#'   \item{Average}{R function(s) to calculate "average" value for multiple samples}
#'   \item{Condition}{R logical expression to test required condition}
#'   \item{LowerLimit}{Value or R expression to calculate lower limit}
#'   \item{UpperLimit}{Value or R expression to calculate upper limit}
#'   \item{Units}{units for guideline (permitted values: mg/L, ug/L, /dL, m, NTU, pH)}
#' }
"limits"
