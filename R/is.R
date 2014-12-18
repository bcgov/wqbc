#' Test wq_thresholds
#'
#' Test if object is of class wq_thresholds
#'
#' @param x object to test
#' @return flag indicating result
#' @export
is_wq_thresholds <- function (x) {
  inherits(x, "wq_thresholds")
}

#' Test wq_index
#'
#' Test if object is of class wq_thresholds
#'
#' @param x object to test
#' @return flag indicating result
#' @export
is_wq_index <- function (x) {
  inherits(x, "wq_index")
}
