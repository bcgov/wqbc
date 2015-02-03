# used by data-raw scripts
strip_ems_codes <- function (x) {
  assert_that(is.character(x) || is.factor(x))
  x <- as.character(x)
  x <- gsub("_", "-", x)
  sub("^EMS[-]", "", x)
}

wqbc_codes <- function () {
  codes <- codes
  codes$Code <- as.character(codes$Code)
  codes$Variable <- as.character(codes$Variable)
  codes$Units <- as.character(codes$Units)
  codes
}

#' Get Codes
#'
#' Gets the recognised water quality codes.
#'
#' @param variables An optional character vector of variables to get codes for.
#' @param messages A flag indicating whether to print messages.
#' @examples
#' get_codes()
#' get_codes(c(get_variables()[1:3], "Kryptonite"))
#' @export
get_codes <- function (
  variables = NULL, messages = getOption("wqbc.messages", default = TRUE)) {
  if(is.null(variables)) return (wqbc_codes()$Code)

  assert_that(is.character(variables) || is.factor(variables))
  variables <- as.character(variables)

  d <- dplyr::left_join(data.frame(Variable = variables, stringsAsFactors = FALSE),
                        wqbc_codes(), by = "Variable")

  if(messages) messages_match_substitution(variables, d$Code, "replace")

  as.character(d$Code)
}
