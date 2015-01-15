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
#' @param add_na flag indicating whether to replace variables without codes
#' with NAs
#'@examples
#' get_codes()
#' variables <- c("Silver", "Kryptonite", "Arsenic", NA, "pH", "Total Phosphorus")
#' get_codes(variables)
#' get_codes(variables, add_na = FALSE)
#' @export
get_codes<- function (variables = NULL, add_na = TRUE) {
  assert_that(is.null(variables) || is.character(variables) || is.factor(variables))
  assert_that(is.flag(add_na) && noNA(add_na))

  if(is.null(variables)) return (as.character(wqbc::codes$Code))

  variables <- as.character(variables)
  x <- dplyr::left_join(data.frame(Variable = variables), wqbc::codes, by = "Variable")
  x$Code <- as.character(x$Code)
  x$Variable <- as.character(x$Variable)
  if(!add_na) {
    bol <- is.na(x$Code)
    x$Code[bol] <- x$Variable[bol]
  }
  x$Code
}

#' Get Category Colours
#'
#' Returns a named vector of the category colours to use when
#' plotting water quality index values.
#' @param palette string indicating palette to use. Possible values are
#' "default" and "blue"
#' @return named character vector of colours for water quality index categories
#' @seealso \code{\link{calc_wqis}}
#' @examples
#' get_category_colours()
#' @export
get_category_colours <- function (palette = "default") {
  assert_that(is.string(palette))

  palettes <- c("default", "blue")
  if(!palette %in% palettes)
    stop("palette must be ", punctuate_strings(palettes))

  if(palette == "blue") return (c(Excellent = "#08306b", Good = "#2171b5", Fair = "#6baed6", Marginal = "#c6dbef", Poor = "#f7fbff"))

  c(Excellent = "#081d58", Good = "#225ea8", Fair = "#41b6c4", Marginal = "#c7e9b4", Poor = "#edf8b1")
}
