#' Geometric Mean Plus-Minus 1
#'
#' Calculates geometric mean by adding 1 before logging
#' and subtracting 1 before exponentiating so that
#' geometric mean of
#' @param x A numeric vector of non-negative numbers.
#' @param na.rm A flag indicating whether to remove missing values.
#' @examples
#' mean(0:9)
#' geomean1(0:9)
#' @export
geomean1 <- function (x, na.rm = FALSE) {
  assert_that(is.vector(x))
  assert_that(is.flag(na.rm) && noNA(na.rm))
  x <- as.numeric(x)

  if(any(x < 0, na.rm = TRUE))
    stop("x must not be negative")

  expm1(mean(log1p(as.numeric(x)), na.rm = na.rm))
}
