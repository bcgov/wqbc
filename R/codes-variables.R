#' Compress EMS Codes
#'
#' Compresses EMS codes by removing EMS_ from
#' start and replacing all '_' with '-'.
#'
#' @param x A character vector of codes to compress.
#' @examples
#' compress_ems_codes(c("EMS_0014", "EMS_KR-P", "0-15"))
#' @export
compress_ems_codes <- function (x) {
  assert_that(is.character(x) || is.factor(x))
  x <- as.character(x)
  x <- gsub("[_]", "-", x)
  sub("^EMS[-]", "", x)
}

#' Expand EMS Codes
#'
#' Expands EMS codes by adding EMS_ to start if absent
#' and replacing all '-' with '_'.
#'
#' @param x A character vector of codes to expand
#' @examples
#' expand_ems_codes(c("0014", "KR-P", "0_15", "EMS_ZN_T"))
#' @export
expand_ems_codes <- function (x) {
  assert_that(is.character(x) || is.factor(x))
  x <- as.character(x)
  x <- gsub("[-]", "_", x)
  bol <- !grepl("^EMS[_-]", x)
  x[bol] <- paste0("EMS_", x[bol])
  x
}

wqbc_codes <- function (compress = FALSE) {
  codes <- codes
  codes$Code <- as.character(codes$Code)
  if(compress)
    codes$Code <- compress_ems_codes(codes$Code)
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
  if(is.null(variables)) return (wqbc_codes(compress = TRUE)$Code)

  assert_that(is.character(variables) || is.factor(variables))
  variables <- as.character(variables)

  d <- dplyr::left_join(data.frame(Variable = variables, stringsAsFactors = FALSE),
                        wqbc_codes(compress = TRUE), by = "Variable")

  if(messages) messages_match_substitution(variables, d$Code, "replace")

  as.character(d$Code)
}

wqbc_limits <- function () {
  limits <- limits
  limits$Variable <- as.character(limits$Variable)
  limits$Units <- as.character(limits$Units)
  limits
}

#' Get Variables
#'
#' Gets recognised water quality variables.
#' Returns character vector of the water quality variables.
#'
#' @param codes An optional character vector of the codes to get the variables for.
#' @param messages A flag indicating whether to print messages.
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
