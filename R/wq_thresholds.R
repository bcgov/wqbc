#' Water Quality Thresholds
#'
#' Gets water quality thresholds for Parameters
#'
#' @param x data.frame with columns Parameter and Value
#' @export
wq_thresholds <- function (x) {
  if("Parameter" %in% colnames(x))
    stop("data.frame x missing Parameter column")
  if("Value" %in% colnames(x))
    stop("data.frame x missing Value column")
}
