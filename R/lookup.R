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

if_null_NA <- function (x) {
  ifelse(is.null(x), NA, x)
}

setup_condition_values <- function (codes, ph, hardness, chloride, methyl_mercury) {

  codes$Value[codes$Variable == "pH"] <- if_null_NA(ph)
  codes$Value[codes$Variable == "Hardness Total"] <- if_null_NA(hardness)
  codes$Value[codes$Variable == "Chloride Total"] <- if_null_NA(chloride)
  codes$Value[codes$Variable == "Mercury Methyl"] <- if_null_NA(methyl_mercury)

  dplyr::filter_(codes, ~!is.na(Value))
}

setup_codes <- function () {
  codes <- wqbc_codes()
  codes$Date <- as.Date("2000-01-01")
  codes$Value <- 1
  dplyr::select_(codes, ~Date, ~Variable, ~Value, ~Units)
}

tidyup_limits <- function (x) {
  x <- dplyr::select_(x, ~Variable, ~UpperLimit, ~Units)
  x$Variable <- factor(x$Variable, levels = lookup_variables())
  x$Units <- factor(x$Units, levels = lookup_units())
  x <- dplyr::arrange_(x, ~Variable)
  x
}

add_missing_limits <- function (x, term) {
  limits <- wqbc_limits()
  limits <- dplyr::filter_(limits, ~tolower(Term) == tolower(term))
  limits <- dplyr::filter_(limits, ~!Variable %in% x$Variable)
  limits <- dplyr::select_(limits, ~Variable, ~Units)
  if(!nrow(limits))
    return (x)
  limits <- unique(limits)
  limits$UpperLimit <- NA_real_
  plyr::rbind.fill(x, limits)
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
lookup_limits <- function (ph = NULL, hardness = NULL, chloride = NULL,
                           methyl_mercury =  NULL, term = "long") {
  assert_that(is.null(ph) || (is.number(ph) && noNA(ph)))
  assert_that(is.null(hardness) || (is.number(hardness) && noNA(hardness)))
  assert_that(is.null(chloride) || (is.number(chloride) && noNA(chloride)))
  assert_that(is.null(methyl_mercury) || (is.number(methyl_mercury) && noNA(methyl_mercury)))
  assert_that(is.string(term))

  if(!term %in% c("short", "long")) stop("term must be \"short\" or \"long\"")

  codes <- setup_codes()
  codes <- setup_condition_values(codes, ph = ph, hardness = hardness,
                                  chloride = chloride, methyl_mercury = methyl_mercury)

  if(term == "long") {
    dates <- codes$Date
    codes <- rbind(codes, codes, codes, codes, codes)
    codes$Date <- c(dates, dates + 1, dates + 2, dates + 3, dates + 21)
  }

  limits <- calc_limits(codes, term = term, messages = FALSE)
  limits <- add_missing_limits(limits, term = term)
  limits <- tidyup_limits(limits)
  limits
}
