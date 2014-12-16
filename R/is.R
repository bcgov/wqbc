#' @export
is.wq_thresholds <- function (x) {
  is.inherits(x, "wq_thresholds")
}

#' @export
is.wq_index <- function (x) {
  is.inherits(x, "wq_index")
}
