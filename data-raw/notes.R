
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
