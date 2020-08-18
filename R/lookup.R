# Copyright 2015 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

#' Lookup Units
#'
#' Returns a character vector of the recognised units.
#'
#' @examples
#' lookup_units()
#' @seealso \code{\link{lookup_limits}}
#' @export
lookup_units <- function() {
  c(
    "ng/L", "ug/L", "mg/L", "g/L", "kg/L", "pH", "degC", "C",
    "CFU/dL", "MPN/dL", "CFU/100mL", "MPN/100mL", "CFU/g", "MPN/g", "CFU/mL", "MPN/mL",
    "Col.unit", "Rel", "NTU", "m", "mS/cm"
  )
}

#' Lookup Use
#'
#' Returns a character vector of the recognised uses.
#'
#' @examples
#' lookup_use()
#' @seealso \code{\link{lookup_limits}}
#' @export
lookup_use <- function() {
  unique(wqbc_limits()$Use)
}

#' Lookup Codes
#'
#' Returns compressed recognised water quality EMS codes.
#' If \code{variables = NULL} the function returns all recognised codes.
#' Otherwise it first substitutes the provided variables for recognised
#' variables using \code{\link{substitute_variables}} and then
#' looks up the matching codes from \code{\link{codes}}.
#'
#' @param variables An optional character vector of variables to lookup codes.
#' @param messages A flag indicating whether to print messages.
#' @examples
#' lookup_codes()
#' lookup_codes(c("Aluminum", "Arsenic Total", "Boron Something", "Kryptonite"),
#'   messages = TRUE
#' )
#' @seealso \code{\link{lookup_limits}} and \code{\link{expand_ems_codes}}
#' @export
lookup_codes <- function(
                         variables = NULL, messages = getOption("wqbc.messages", default = TRUE)) {
  if (is.null(variables)) {
    return(wqbc_codes(compress = TRUE)$Code)
  }

  variables <- substitute_variables(variables, messages = messages)
  d <- dplyr::left_join(data.frame(Variable = variables, stringsAsFactors = FALSE),
    wqbc_codes(compress = TRUE),
    by = "Variable"
  )
  if (messages) messages_match_substitution(variables, d$Code, "replace")

  as.character(d$Code)
}

#' Lookup Variables
#'
#' Returns recognised water quality variables.
#' If \code{codes = NULL} the function returns all recognised variable names.
#' Otherwise it
#' looks up the matching variables from \code{\link{codes}}. Whether or
#' not the codes are compressed or expanded is unimportant.
#'
#' @param codes An optional character vector of codes to look up variables.
#' @param messages A flag indicating whether to print messages.
#' @examples
#' lookup_variables()
#' lookup_variables(c("AL-D", "EMS_AS_T", "B--T", "KRYP"), messages = TRUE)
#' @seealso \code{\link{lookup_limits}} and \code{\link{expand_ems_codes}}
#' @export
lookup_variables <- function(
                             codes = NULL, messages = getOption("wqbc.messages", default = TRUE)) {
  if (is.null(codes)) {
    return(wqbc_codes()$Variable)
  }

  chkor(chk_character(codes), chk_s3_class(codes, "factor"))
  codes <- as.character(codes)
  codes <- compress_ems_codes(codes)
  d <- dplyr::left_join(data.frame(Code = codes, stringsAsFactors = FALSE),
    wqbc_codes(compress = TRUE),
    by = "Code"
  )
  if (messages) messages_match_substitution(codes, d$Variable, "replace")
  as.character(d$Variable)
}

if_null_NA <- function(x) {
  ifelse(is.null(x), NA, x)
}

setup_condition_values <- function(codes, ph, hardness, chloride, methyl_mercury) {
  codes$Value[codes$Variable == "pH"] <- if_null_NA(ph)
  codes$Value[codes$Variable == "Hardness Total"] <- if_null_NA(hardness)
  codes$Value[codes$Variable == "Chloride Total"] <- if_null_NA(chloride)
  codes$Value[codes$Variable == "Mercury Methyl"] <- if_null_NA(methyl_mercury)

  dplyr::filter(codes, !is.na(.data$Value))
}

setup_codes <- function() {
  codes <- wqbc_codes()
  codes$Date <- as.Date("2000-01-01")
  codes$Value <- 1
  dplyr::select(codes, .data$Date, .data$Variable, .data$Value, .data$Units)
}

tidyup_limits <- function(x) {
  x <- dplyr::select(x, .data$Variable, .data$UpperLimit, .data$Units)
  x$Variable <- factor(x$Variable, levels = lookup_variables())
  x$Units <- factor(x$Units, levels = lookup_units())
  x <- dplyr::arrange(x, .data$Variable)
  x
}

add_missing_limits <- function(x, term) {
  limits <- wqbc_limits()
  limits <- dplyr::filter(limits, tolower(.data$Term) == tolower(term))
  limits <- dplyr::filter(limits, !.data$Variable %in% x$Variable)
  limits <- dplyr::select(limits, .data$Variable, .data$Units)
  if (!nrow(limits)) {
    return(x)
  }
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
#' @param use A string indicating the Use.
#' @examples
#' lookup_limits(ph = 8, hardness = 100, chloride = 50, methyl_mercury = 2)
#' lookup_limits(term = "short")
#' @seealso \code{\link{calc_limits}}
#' @export
lookup_limits <- function(ph = NULL, hardness = NULL, chloride = NULL,
                          methyl_mercury =  NULL, term = "long",
                          use = "Freshwater Life") {
  chkor(chk_null(ph), check_values(ph, 1))
  chkor(chk_null(hardness), check_values(hardness, 1))
  chkor(chk_null(chloride), check_values(chloride, 1))
  chkor(chk_null(methyl_mercury), check_values(methyl_mercury, 1))
  chk_string(term)

  term <- tolower(term)
  if (!term %in% c("short", "long")) stop("term must be \"short\" or \"long\"")

  codes <- setup_codes()
  codes <- setup_condition_values(codes,
    ph = ph, hardness = hardness,
    chloride = chloride, methyl_mercury = methyl_mercury
  )

  if (term == "long") {
    dates <- codes$Date
    codes <- rbind(codes, codes, codes, codes, codes)
    codes$Date <- c(dates, dates + 1, dates + 2, dates + 3, dates + 21)
  }

  limits <- calc_limits(codes, term = term, keep_limits = FALSE, messages = FALSE, use = use)
  limits <- add_missing_limits(limits, term = term)
  limits <- tidyup_limits(limits)
  tibble::as_tibble(limits)
}
