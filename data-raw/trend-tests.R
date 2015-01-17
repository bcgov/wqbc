#' Test for Trends in Water Quality Indices
#'
#' Tests for linear and non-linear trends in water quality indices
#'
#' [DRAFT]
#'
#' If the argument \code{x} must have columns:
#' WQI (0 to 100),
#' Year (or whichever single column is specified by the argument Year).
#'
#' only one WQI value for each year is allowed.
#' If a by argument is specified for example if by = c("Lake", "Site")
#' then a trend test is conducted for each combination of Site
#' within each Lake by Year
#'
#' The function returns an object of class wq_trend which inherits from data.frame. It
#' the will include any columns listed in the \code{by} argument plus any columns which do
#' not vary for each Trend test (allows things like Lat and Lon to be passed
#' through for plotting particular sites) plus the columns From and To which give
#' the first and last Year for each trend test plus the column Values which gives
#' the actual number of WQI values in the test plus the columns Trend (which
#' gives the positive or negative net rate of change per scale unit) and
#' Significance (p-value).
#'
#' @param x data.frame with columns WQI and time defined in \code{scale}
#' @param scale the time stamp of the observation which must be a named column of \code{x}
#' @param by a factor or a list of factors, which must be named columns of  \code{x}.
#' @export
#' @examples
#' # subset data to select only Pb and SiteID "BC08KA0001"
#' library(lubridate)
#' data(fraser)
#' x <- subset(fraser, SiteID == "BC08KA0001" & grepl("Pb", Code))
#'
#' # rescale recorded values to be on common scale (some are mg/L some ug/L)
#' x $ WQI <- x $ Value * ifelse(x $ Units == "mg/L", 1000, 1)
#' # and month and year covariates
#' x $ month <- factor(month.abb[month(x $ Date)], levels = month.abb)
#' x $ year <- year(x $ Date)
#'
#' test_trends(x, scale = "year", by = "month")
test_trends <- function (x, scale = "Year", by = NULL) {

  # center the time variable
  mean_scale <- mean(x[[scale]])
  x $ centered_scale <- x[[scale]] - mean_scale

  # form by indices
  INDICES <- do.call(paste, x[by])

  # run the trend test
  fit <- by(x,  INDICES, function(x.) zyp.trend.vector(y = x. $ WQI, x = x. $ centered_scale, method = "yuepilon"))

  # return this list for now, but we will be producing
  # a specific class in coming revisions
  unclass(fit)
}
