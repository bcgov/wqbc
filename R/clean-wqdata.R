#' Clean Water Quality Data
#'
#' Cleans water quality data.
#'
#' @param x data.frame with columns Date, Variable, Value and Units
#' @param messages flag indicating whether to print messages
#' @examples
#' calc_rcvs(dummy, messages = TRUE)
#' @export
clean_wqdata <- function (x, messages = getOption("wqbc.messages", default = TRUE)) {
  assert_that(is.data.frame(x))
  assert_that(is.flag(messages) && noNA(messages))

  check_columns(x, c("Variable", "Value", "Units", "Date"))

  check_class_columns(x, list("Variable" = c("character", "factor"),
                              "Value" = "numeric",
                              "Units" = c("character", "factor"),
                              "Date" = "Date"))

  if(!nrow(x)) return (x)

  x$Variable <- substitute_variables(x$Variable, messages = messages)
  x$Units <- substitute_units(x$Units, messages = messages)

  is.na(x$Variable[!x$Variable %in% get_variables()]) <- TRUE
  is.na(x$Units[!x$Units %in% get_units()]) <- TRUE

  x$Value <- replace_negative_values_with_na(x$Value, messages = messages)

  x <- delete_rows_with_missing_values(x, messages = FALSE)

  x <- standardise_values_units(x, messages = messages)

  x <- average_replicates(x, messages = message)

  x <- delete_rows_with_missing_values(x, messages = FALSE)

  x
}
