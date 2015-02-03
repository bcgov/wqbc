#' Get Units
#'
#' Gets a character vector of the recognised units.
#'
#' @examples
#' get_units()
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

#' Substitute Units
#'
#' Where possible substitute units with
#' recognised values. Returns a character vector of the substituted or original units.
#'
#' @param x The character vector of units to substitute.
#' @param messages A flag indicating whether to print messages.
#' @examples
#' substitute_units(c("mg/L", "MG/L", "mg /L ", "Kg/l", "gkl", "CFU/100ML"))
#' substitute_units(c("MG/L", "MG/L", "MG/L"))
#' substitute_units("gkl")
#' substitute_units(c(NA, "mg/L"))
#' @export
substitute_units <- function (
  x, messages = getOption("wqbc.messages", default = TRUE)) {
  assert_that(is.character(x) || is.factor(x))

  x <- as.character(x)

  y <- gsub("units", "", x, ignore.case = TRUE)
  y <- gsub(" ", "", y)
  y <- gsub("100mL", "dL", y, ignore.case = TRUE)

  wqbc_substitute(x, y, sub = get_units(), messages)
}
