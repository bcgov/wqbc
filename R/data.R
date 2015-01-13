#' CCME Water Quality Index User's Manual Example Data
#'
#' A tidy data.frame of the CCME Water Quality Index 1.0 User's Manual
#' example dataset.
#'
#' @format A data frame with 103 rows and 6 variables:
#' \describe{
#'   \item{Variable}{parameter variable name}
#'   \item{Date}{date of reading}
#'   \item{Value}{value of reading}
#'   \item{DetectionLimit}{detection limit of method}
#'   \item{LowerLimit}{minimum permitted value}
#'   \item{UpperLimit}{maximum permitted value}
#' }
#' @examples
#' library(tidyr)
#'
#' data(ccme)
#' ccme$Code <- get_codes(ccme$Variable, add_na = FALSE)
#' ccme$Code[ccme$Code == "Fecal Coliforms"] <- "FC"
#' ccme$Code <- factor(ccme$Code, levels = unique(ccme$Code))
#' tidyr::spread(dplyr::select(ccme, Code, Value, Date), "Code", "Value")
#'
#' library(ggplot2)
#'
#' wqis <- calc_wqis(ccme)
#' plot_wqis(wqis)
#'
#' wqis <- calc_wqis(ccme, by = "Date")
#' plot_wqis(wqis)
#' plot_wqis(wqis, x = "Date")
#'
#' library(lubridate)
#'
#' wqis$Year <- year(wqis$Date)
#' wqis$Dayte <- wqis$Date
#' year(wqis$Dayte) <- 2000
#' plot_wqis(wqis, x = "Dayte", size = "Tests") +
#' facet_wrap(~Year) + xlab("Day of the Year") + theme_bw()
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
#' @format A data frame with 9 variables:
#' \describe{
#'   \item{SiteID}{unique water quality station number}
#'   \item{Date}{date of water quality sample}
#'   \item{Variable}{variable name}
#'   \item{Value}{measured value}
#'   \item{Units}{reading units}
#'   \item{DetectionLimit}{minimum value of method}
#'   \item{Site}{full station name}
#'   \item{Lat}{latitude in decimal degrees}
#'   \item{Long}{longitude in decimal degrees}
#' }
#' @source \url{http://open.canada.ca/data/en/dataset/9ec91c92-22f8-4520-8b2c-0f1cce663e18}
"fraser"

#' Water Quality Parameter codes and units
#'
#' @details Water Quality Parameter codes and units
#'
#' @format A data frame with 3 variables:
#' \describe{
#'   \item{Code}{unique short-hand code}
#'   \item{Variable}{unique name of water quality parameter}
#'   \item{Units}{units for parameter}
#' }
"codes"

#' Water Quality Limits for British Columbia and Canada
#'
#' @format A data frame with 13 variables:
#' \describe{
#'   \item{Code}{short-hand code}
#'   \item{Variable}{name of water quality parameter}
#'   \item{Average}{R function(s) to calculate "average" value for multiple samples}
#'   \item{Condition}{R logical expression to test required condition}
#'   \item{LowerLimit}{R expression defining lower limit}
#'   \item{UpperLimit}{R expression defining upper limit}
#'   \item{Units}{units for limit}
#' }
"limits"

#' Borders of British Columbia
#'
#' Borders of British Columbia used by \code{\link{plot_map}} function.
#'
#' @format A data frame with 4,920 rows and 3 variables:
#' \describe{
#'   \item{Long}{longitude}
#'   \item{Lat}{latitude}
#'   \item{Group}{grouping variable for plotting to ensure discrete polygons}
#' }
"map"
