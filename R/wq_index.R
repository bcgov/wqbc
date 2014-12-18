#' Water Quality Index
#'
#' Calculates water quality index from data.frame with columns Parameter, Value
#' and Threshold.
#'
#' @param x data.frame with columns Parameter, Value and Threshold
wq_index <- function (x) {
  if("Parameter" %in% colnames(x))
    stop("data.frame x missing Parameter column")
  if("Value" %in% colnames(x))
    stop("data.frame x missing Value column")
  if("Parameter" %in% colnames(x))
    stop("data.frame x missing Parameter column")
}
