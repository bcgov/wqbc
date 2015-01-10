## NOTE THIS FILE IS SOURCED BY SCRIPT data-raw/codes.R TO GET UNITS
#
#' Get Units
#'
#' @return character vector of the recognised units.
#' @examples
#' get_units()
#'
#' @export
get_units <- function () {
  c("ug/L", "mg/L", "g/L", "kg/L",
    "mm", "cm", "m", "km",
    "/100mL", "pH", "NTU")
}

get_unit_multiplier <- function (x) {
  units <- c("ug/L" = 10^-6, "mg/L" = 10^-3, "g/L" = 1,  "kg/L" = 10^3,
             "mm" = 10^-3, "cm" = 10^-2, "m" = 1, "km" = 10^3,
             "/100mL" = 1,  "pH" = 1, "NTU" = 1)
  x <- units[x]
  names(x) <- NULL
  x
}

get_unit_type <- function (x) {
  type <- list("concentration" = c("ug/L", "mg/L", "g/L", "kg/L"),
               "length" = c("mm", "cm", "m", "km"),
               "individuals" = "/100mL",
               "pH" = "pH",
               "turbidity" = "NTU")

  type <- unlist(type)
  names <- sub("\\d$", "", names(type))
  values <- type
  type <- names
  names(type) <- values

  x <- type[x]
  names(x) <- NULL
  x
}

#' Substitute Units
#'
#' Where possible substitute units with
#' possible values
#'
#' @param x character vector of units to substitute
#' @return character vector of substituted units where
#' match or NA
#' @examples
#' substitute_units(c("mg/L", "MG/L", "mg /L ", "Kg/l"))
#' @export
substitute_units <-function (x) {
  assert_that(is.character(x) || is.factor(x))

  x <- as.character(x)
  x <- tolower(x)
  x <- gsub(" ", "", x)
  units <- get_units()

  bol <- !is.na(x) & !x %in% tolower(units)
  if(any(bol)) {
    warning("The following units are unrecognised and are replaced
              with a missing value: ",
            punctuate_strings(unique(x[bol]), "and"), ".
              To see possible units type get_units()")
    is.na(x[bol]) <- TRUE
  }
  if(all(is.na(x)))
    return (x)

  x <- data.frame(x = x)
  units <- data.frame(x = tolower(units), units = units)

  x <- dplyr::left_join(x, units, by = "x")
  x <- as.character(x$units)
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
#' convert_units(1:10, from = "mg/L", to = "ug/L")
#' @export
convert_units <- function (x, from, to) {
  assert_that(is.numeric(x))
  assert_that(is.character(from) || is.factor(from))
  assert_that(is.character(to) || is.factor(to))

  from <- substitute_units(from)
  to <- substitute_units(to)

  x <- x * get_unit_multiplier(from) / get_unit_multiplier(to)

  bol <- get_unit_type(from) != get_unit_type(to)

  if(any(bol, na.rm = TRUE)) {
    warning(sum(bol, na.rm = TRUE), " values have inconvertible units")
    is.na(x[!is.na(bol) & bol]) <- TRUE
  }
  x
}
