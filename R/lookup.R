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
