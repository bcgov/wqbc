standardize_wqdata_variable <- function (x, messages) {
  codes <- wqbc_codes()
  codes <- dplyr::filter_(codes, ~Variable == x$Variable[1])
  x$Value <- convert_values(x$Value, from = x$Units, to = codes$Unit,
                            messages = messages)
  x$Units <- codes$Unit
  x
}

#' Standardize Water Quality Data
#'
#' Standardizes a water quality data set so that only recognised
#' variables and units remain and values have consistent units.
#' Negative or missing values are removed. When strict = FALSE
#' ambiguous variables such as "Iron Dissolved"
#' and "Iron Total" are dropped.
#'
#' @param x The data.frame to standardize.
#' @param strict A flag indicating whether to require all words
#' in a variable name to be present or only the first word.
#' @param messages A flag indicating whether to print messages.
#' @examples
#' standardize_wqdata(wqbc::dummy, messages = TRUE)
#' @export
standardize_wqdata <- function (
  x, strict = TRUE, messages = getOption("wqbc.messages", default = TRUE)) {
  assert_that(is.data.frame(x))
  assert_that(is.flag(strict) && noNA(strict))
  assert_that(is.flag(messages) && noNA(messages))

  if("Code" %in% colnames(x)) {
    if(messages) message ("Converting Codes to Variables...")
    x$Variable <- get_variables(x$Code, messages = messages)
    x <- delete_rows_with_certain_values(
      x, columns = c("Variable"), messages = messages)
    if(messages) message ("Converted Codes to Variables.")
  }

  if(messages) message("Standardizing water quality data...")

  check_columns(x, c("Variable", "Value", "Units"))

  if(is.factor(x$Variable)) x$Variable <- as.character(x$Variable)
  if(is.factor(x$Units)) x$Units <- as.character(x$Units)

  check_class_columns(x, list("Variable" = "character",
                              "Value" = "numeric",
                              "Units" = "character"))

  if(!nrow(x)) { message("Standardized."); return (x) }

  x$Variable <- substitute_variables(x$Variable, strict = strict, messages = messages)
  x$Units <- substitute_units(x$Units, messages = messages)

  is.na(x$Variable[!x$Variable %in% get_variables()]) <- TRUE
  is.na(x$Units[!x$Units %in% get_units()]) <- TRUE

  x$Value <- replace_negative_values_with_na(x$Value, messages = messages)

  x <- delete_rows_with_certain_values(x, columns = c("Variable", "Value", "Units"),
                                       messages = messages)

  if(!nrow(x)) { message("Standardized."); return (x) }

  x <- plyr::ddply(x, .variables = "Variable",
                   .fun = standardize_wqdata_variable, messages = messages)

  message("Standardized water quality data.")

  x
}
