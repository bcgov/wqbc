
#' @export
as.wq_thresholds <- function (x) {
  x <- as.data.frame(x)

  if(!is.data.frame(x))
    stop("x is not a data.frame")

  if(!"Parameter" %in% colnames(x))
    stop("x must include column 'Parameter'")

  if(!"Parameter" %in% colnames(x))
    stop("x must include column 'Parameter'")

  class (x) <- c("wq_thresholds", "data.frame")
  x
}
