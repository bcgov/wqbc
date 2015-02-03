wqbc_limits <- function () {
  limits <- limits
  limits$Variable <- as.character(limits$Variable)
  limits$Units <- as.character(limits$Units)
  limits
}

#' Get Variables
#'
#' Gets recognised water quality variables.
#'
#' @param codes An optional character vector of the codes to get the variables for.
#' @param messages A flag indicating whether to print messages.
#' @return A character vector of the water quality variables.
#' @examples
#' get_variables()
#' get_variables(get_codes())
#' get_variables(c(get_codes()[1:3], "KRYP"))
#' @export
get_variables<- function (
  codes = NULL, messages = getOption("wqbc.messages", default = TRUE)) {
  if(is.null(codes)) return (wqbc_codes()$Variable)

  assert_that(is.character(codes) || is.factor(codes))
  codes <- as.character(codes)

  codes <- compress_ems_codes(codes)

  d <- dplyr::left_join(data.frame(Code = codes, stringsAsFactors = FALSE),
                        wqbc_codes(compress = TRUE), by = "Code")

  if(messages) messages_match_substitution(codes, d$Variable, "replace")

  as.character(d$Variable)
}
