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
    "/dL", "pH", "NTU")
}

get_unit_multiplier <- function (x) {
  units <- c("ug/L" = 10^-6, "mg/L" = 10^-3, "g/L" = 1,  "kg/L" = 10^3,
             "mm" = 10^-3, "cm" = 10^-2, "m" = 1, "km" = 10^3,
             "/dL" = 1,  "pH" = 1, "NTU" = 1)
  x <- units[x]
  names(x) <- NULL
  x
}

get_unit_type <- function (x) {

    type <- list("concentration" = c("ug/L", "mg/L", "g/L", "kg/L"),
                 "length" = c("mm", "cm", "m", "km"),
                 "individuals" = "/dL",
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

#' Get Water Quality Uses
#'
#' Returns a character vector of the uses for which
#' limits are currently defined in the wqbc package.
#' @examples
#' get_uses()
#'
#' @export
get_uses <- function () {
  levels(wqbc::limits$Use)
}

#' Get Water Quality Variables
#'
#' Returns a character vector of the water quality variables for which
#' limits are currently defined in the wqbc package.
#' @param codes optional character vector of codes to get variables for
#' @examples
#' get_variables()
#' get_variables(c("Ag", "KR", "As", NA, "pH", "TP"))
#'
#' @export
get_variables<- function (codes = NULL) {
  if(is.null(codes)) return (levels(wqbc::limits$Variable))

  assert_that(is.character(codes) || is.factor(codes))
  codes <- as.character(codes)
  x <- data.frame(Code = codes)
  y <- get_codes_variables()
  x <- dplyr::left_join(x,y, by = "Code")
  x$Variable <- as.character(x$Variable)
  x$Variable
}

#' Get Water Quality Codes
#'
#' Returns a character vector of the water quality codes for which
#' limits are currently defined in the wqbc package.
#'
#' @param variables optional character vector of variables to get codes for
#'@examples
#' get_codes()
#' get_codes(c("Silver", "Kryptonite", "Arsenic", NA, "pH", "Total Phosphorus"))
#'
#' @export
get_codes<- function (variables = NULL) {
  if(is.null(variables)) return (levels(wqbc::limits$Code))

  assert_that(is.character(variables) || is.factor(variables))
  variables <- as.character(variables)
  x <- data.frame(Variable = variables)
  y <- get_codes_variables()
  x <- dplyr::left_join(x,y, by = "Variable")
  x$Code <- as.character(x$Code)
  x$Code
}

#' Get Water Quality Code-Variable Lookup
#'
#' Returns a data.frame of the water quality code and variable
#' look up table currently defined in the wqbc package.
#' @examples
#' get_codes_variables()
#'
#' @export
get_codes_variables <- function () {
  x <- dplyr::select_(wqbc::limits, ~Code, ~Variable)
  x <- unique(x)
  dplyr::arrange_(x, ~Code)
}

#' Get Category Colours
#'
#' Returns a named vector of the category colours to use when
#' plotting water quality index values.
#' @seealso \code{\link{calc_wqis}}
#' @examples
#' get_category_colours()
#'
#' @export
get_category_colours <- function () {
  c(Excellent = "green", Good = "blue", Fair = "yellow", Marginal = "brown", Poor = "red")
}
