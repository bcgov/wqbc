#' Get Water Quality Uses
#'
#' Returns a character vector of the uses for which
#' guidelines are currently defined in the wqbc package.
#'
#' @export
wq_uses <- function () {
  c("Drinking", "Freshwater Life", "Marine Life", "Wildlife", "Livestock",
    "Irrigation", "Recreation")
}

#' Get Water Quality Jurisdictions
#'
#' Returns a character vector of the jurisdictions for which
#' guidelines are currently defined in the wqbc package.
#'
#' @export
wq_jurisdictions <- function () {
  c("BC", "CA")
}

#' Get Water Quality Variables
#'
#' Returns a character vector of the water quality variables for which
#' guidelines are currently defined in the wqbc package.
#'
#' @export
wq_variables <- function () {
  sort(wqbc::guidelines$Variables)
}
