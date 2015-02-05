#' Convert EMS Data
#'
#' Converts EMS data.
#'
#' @param x The data.frame to convert.
#' @param date A string of Date column name.
#' @param code A string of Code column name.
#' @param value A string of Value column name.
#' @param units A string of Units column name.
#' @param messages A flag indicating whether to print messages.
#' @examples
#'
#' data <- data.frame(date = as.Date("2000-01-01"), code = c("SE-T", "KRYP", "0004"),
#' value = 1, units = c("MG/L","MG/L","PH UNITS"))
#' convert_ems_data(data)
#'
#' @export
convert_ems_data <- function (x, date = "date", code = "code",
                              value = "value", units = "units",
                              messages = getOption("wqbc.messages", default = TRUE)) {
  assert_that(is.data.frame(x))
  assert_that(is.string(date))
  assert_that(is.string(code))
  assert_that(is.string(value))
  assert_that(is.string(units))

  if(messages) message("Converting EMS data...")

  check_columns(x, c(date, code, value, units))

  x <- x[c(date, code, value, units)]
  colnames(x) <- c("Date", "Code", "Value", "Units")
  x$Date <- as.Date(x$Date)

  x <- delete_rows_with_certain_values(
    x, columns = c("Date", "Code", "Value", "Units"), messages = messages)

  x$Variable <- get_variables(x$Code, messages = messages)
  x$Units <- substitute_units(x$Units, messages = messages)
  x <- delete_rows_with_certain_values(
    x, columns = c("Date", "Variable", "Value", "Units"), messages = messages,
    txt = "unrecognised"
  )
  x$Variable <- factor(x$Variable)
  x$Code <- factor(x$Code)
  if(messages) message("Converted.")
  dplyr::select_(x, ~Date, ~Variable, ~Value, ~Units)
}
