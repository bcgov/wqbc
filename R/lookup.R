#' Get Water Quality Uses
#'
#' Returns a character vector of the uses for which
#' guidelines are currently defined in the wqbc package.
#'
#' @export
wq_uses <- function () {
  levels(wqbc::guidelines$Use)
}

#' Get Water Quality Jurisdictions
#'
#' Returns a character vector of the jurisdictions for which
#' guidelines are currently defined in the wqbc package.
#'
#' @export
wq_jurisdictions <- function () {
  levels(wqbc::guidelines$Jurisdiction)
}

#' Get Water Quality Variables
#'
#' Returns a character vector of the water quality variables for which
#' guidelines are currently defined in the wqbc package.
#'
#' @export
wq_variables <- function () {
  levels(wqbc::guidelines$Variable)
}

#' Get Water Quality Codes
#'
#' Returns a character vector of the water quality codes for which
#' guidelines are currently defined in the wqbc package.
#'
#' @export
wq_codes<- function () {
  levels(wqbc::guidelines$Code)
}

#' Get Water Quality Code-Variable Lookup
#'
#' Returns a data.frame of the water quality code and variable
#' look up table currently defined in the wqbc package.
#'
#' @export
wq_code_variable <- function () {
  x <- dplyr::select_(guidelines, ~Code, ~Variable)
  x <- unique(x)
  dplyr::arrange_(x, ~Code)
}
