get_unit_multiplier <- function (x) {
  units <- c("ng/L" = 10^-9, "ug/L" = 10^-6, "mg/L" = 10^-3,
             "g/L" = 1,  "kg/L" = 10^3,
             "pH" = 1)
  x <- units[x]
  names(x) <- NULL
  x
}

get_unit_type <- function (x) {
  type <- list("concentration" = c("ng/L", "ug/L", "mg/L", "g/L", "kg/L"),
               "pH" = "pH")

  type <- unlist(type)
  names <- sub("\\d$", "", names(type))
  values <- type
  type <- names
  names(type) <- values

  x <- type[x]
  names(x) <- NULL
  x
}

convert_values <- function (x, from, to, messages) {

  from <- substitute_units(from, messages = messages)
  to <- substitute_units(to, messages = messages)

  x <- x * get_unit_multiplier(from) / get_unit_multiplier(to)

  bol <- from != to & get_unit_type(from) != get_unit_type(to)
  bol <- is.na(bol) | bol

  if(any(bol)) {
    warning(sum(bol), " values have inconvertible units")
    is.na(x[bol]) <- TRUE
  }
  x
}

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

