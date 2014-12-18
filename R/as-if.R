#' Test if equivalent to wq_thresholds object
#'
#' Tests if object is equvialent to an object of
#'  class wq_thresholds
#'
#' @param x object to test
#' @return flag if successful otherwise an informative error message
#' @export
as_if_wq_thresholds <- function (x) {
  x <- as.data.frame(x)

  if(!is.data.frame(x))
    stop("x must be a data.frame")

  if(nrow(x) == 0)
    stop("x must contain data")

  if(!"Parameter" %in% colnames(x))
    stop("x must have a column 'Parameter'")

  if(!"Value" %in% colnames(x))
    stop("x must have a column 'Value'")

  if(!"Threshold" %in% colnames(x))
    stop("x must have a column 'Threshold'")

  TRUE
}
