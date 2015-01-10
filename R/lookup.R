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
#' Returns a character vector of the water quality variables
#' recognised by the wqbc package.
#' @param codes optional character vector of codes to get variables for
#' @examples
#' get_variables()
#' get_variables(c("Ag", "KR", "As", NA, "pH", "TP"))
#' @export
get_variables<- function (codes = NULL) {
  if(is.null(codes)) return (levels(wqbc::codes$Variable))

  assert_that(is.vector(codes))
  codes <- as.character(codes)

  x <- dplyr::left_join(data.frame(Code = codes), wqbc::codes, by = "Code")
  as.character(x$Variable)
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
  if(is.null(variables)) return (levels(wqbc::codes$Code))

  assert_that(is.vector(variables))
  variables <- as.character(variables)

  x <- dplyr::left_join(data.frame(Variable = variables), wqbc::codes, by = "Variable")
  as.character(x$Code)
}

#' Get Category Colours
#'
#' Returns a named vector of the category colours to use when
#' plotting water quality index values.
#' @seealso \code{\link{calc_wqis}}
#' @examples
#' get_category_colours()
#' @export
get_category_colours <- function () {
  c(Excellent = "green", Good = "blue", Fair = "yellow", Marginal = "brown", Poor = "red")
}
