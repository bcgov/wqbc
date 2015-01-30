#' Get Units
#'
#' @return character vector of the recognised units.
#' @examples
#' get_units()
#'
#' @export
get_units <- function () {
  c("ng/L", "ug/L", "mg/L", "g/L", "kg/L", "pH")
}

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

#' Convert Units
#'
#' Converts units
#'
#' @param x numeric vector of values to convert
#' @param from character vector of original units
#' @param to character vector new units
#' @return numeric vector of values in new units
#' @examples
#' convert_units(1:2, from = "mg/L", to = "mg/L")
#' convert_units(1:2, from = "mg/L", to = "ug/L")
#' convert_units(1:2, from = "mg/L", to = "midichlorians")
#' convert_units(1:2, from = "midichlorians", to = "midichlorians")
#' @export
convert_units <- function (x, from, to) {
  assert_that(is.numeric(x))
  assert_that(is.character(from) || is.factor(from))
  assert_that(is.character(to) || is.factor(to))

  from <- substitute_units(from)
  to <- substitute_units(to)

  x <- x * get_unit_multiplier(from) / get_unit_multiplier(to)

  bol <- from != to & get_unit_type(from) != get_unit_type(to)
  bol <- is.na(bol) | bol

  if(any(bol)) {
    warning(sum(bol), " values have inconvertible units")
    is.na(x[bol]) <- TRUE
  }
  x
}
