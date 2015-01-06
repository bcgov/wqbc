#' Water Quality Thresholds and Index Calculation for British Columbia
#'
#' @docType package
#' @name wqbc
#' @import assertthat ggplot2
#' @import zyp
#' @examples
#' library(ggplot2)
#' data(fraser)
#' plot_map(fraser)
#'
#' data(ccme)
#' calc_wqis(ccme)
#' wqis <- calc_wqis(ccme, by = "Date")
#' plot_wqis(wqis, x = "Date")
#'
NULL
