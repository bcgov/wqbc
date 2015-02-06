#' Lookup Units
#'
#' Looks up a character vector of the recognised units.
#'
#' @examples
#' lookup_units()
#' @export
lookup_units <- function () {
  c("ng/L", "ug/L", "mg/L", "g/L", "kg/L", "pH")
}

#' Get Codes
#'
#' Gets the recognised water quality codes.
#'
#' @param variables An optional character vector of variables to get codes for.
#' @param messages A flag indicating whether to print messages.
#' @examples
#' lookup_codes()
#' lookup_codes(c(lookup_variables()[1:3], "Kryptonite"))
#' @export
lookup_codes <- function (
  variables = NULL, messages = getOption("wqbc.messages", default = TRUE)) {
  if(is.null(variables)) return (wqbc_codes(compress = TRUE)$Code)

  assert_that(is.character(variables) || is.factor(variables))
  variables <- as.character(variables)
  d <- dplyr::left_join(data.frame(Variable = variables, stringsAsFactors = FALSE),
                        wqbc_codes(compress = TRUE), by = "Variable")
  if(messages) messages_match_substitution(variables, d$Code, "replace")

  as.character(d$Code)
}

#' Lookup Variables
#'
#' Looks up recognised water quality variables.
#' Returns character vector of the water quality variables.
#'
#' @param codes An optional character vector of the codes to get the variables for.
#' @param messages A flag indicating whether to print messages.
#' @examples
#' lookup_variables()
#' lookup_variables(lookup_codes())
#' lookup_variables(c(lookup_codes()[1:3], "KRYP"))
#' @export
lookup_variables<- function (
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

#' Lookup Limits
#'
#' Looks up the long or short-term water quality limits for BC. If the limits depend on
#' on the pH, total hardness (CaCO3), total chloride or the concentration of methyl mercury
#' and site specific values are not provided then the dependent limits are returned
#' as missing values.
#'
#' @param ph A number indicating the pH in pH units at the site of interest.
#' @param hardness A number indicating the total hardness (CaCO3) in mg/L at the site of interest.
#' @param chloride A number indicating the total chloride concentration in mg/L at the site of interest.
#' @param methyl_mercury A number indicating the total concentration of methyl mercury in ug/L at the site of interest.
#' @param term A string indicating whether to lookup the "long" or "short"-term limits.
#' @examples
#' lookup_limits(ph = 8, hardness = 100, chloride = 50, methyl_mercury = 2)
#' lookup_limits(ph = 8, hardness = 100, chloride = 50, methyl_mercury = 2, term = "short")
#' @export
lookup_limits <- function (ph = NA_real_, hardness = NA_real_ ,
                           chloride = NA_real_, methyl_mercury =  NA_real_,
                           term = "long") {
  assert_that(is.number(ph))
  assert_that(is.number(hardness))
  assert_that(is.number(chloride))
  assert_that(is.number(methyl_mercury))
  assert_that(is.string(term))

  if(!term %in% c("short", "long")) stop("term must be \"short\" or \"long\"")

  data <- data.frame()
}
