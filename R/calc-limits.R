add_limits_use <- function (x, use) {
  x$..ID <- 1:nrow(x)
  x <- dplyr::rename_(x, "..Units" = "Units")

  y <- dplyr::filter_(wqbc::limits, ~Use == use)
  y <- dplyr::select_(y, ~Code, ~LowerLimit, ~UpperLimit, ~Units, ~Samples, ~Period,
                      ~Condition, ~Variable, ~Use)

  x <- delete_columns(x, colnames(dplyr::select_(y, ~-Code)))
  x <- dplyr::left_join(x, y, by = "Code")

  x$Value <- convert_units(x$Value, from = x$..Units, to = x$Units)
  x$..Units <- NULL
  x
}

#' Calculates Water Quality limits
#'
#' Calculates the approved lower and upper water quality thresholds for
#' British Columbia. If the Date column is not
#' supplied the data is assumed to have been collected on the same date.
#' Assumes values are individual readings.
#'
#' @param x data.frame with columns Code, Value and Units.
#' @param by character vector of columns to calculate limits by
#' @examples
#' data(fraser)
#' fraser <- calc_limits(fraser)
#' @export
calc_limits <- function (x, by = NULL) {
  assert_that(is.data.frame(x))
  assert_that(is.null(by) || (is.character(by) && noNA(by)))


  check_rows(x)
#   check_columns(x, c("Code", "Value", "Units"))
#   x <- add_missing_columns(x, list("Date" = as.Date("2000-01-01")))
#   check_class_columns(x, list("Code" = c("character", "factor"),
#                               "Value" = "numeric",
#                               "Units" = c("character", "factor"),
#                               "Date" = "Date"))
#
#   x$Units <- substitute_units(x$Units)
#   x <- delete_rows_with_missing_values(x, list("Code", "Value", "Units", "Date"))
#   x <- add_limits_use(x, use)
#   x <- delete_rows_with_missing_values(x, list("Value", "Units"))
#
#   # also need to check not in limits....
#   check_by(by, x, res_names = c("Code", "Value", "Units", "Date"))

#  x <- tidy_up_values (x)
  x
}
