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

calc_limit <- function (x) {
  #convert_units..
  x <- x[,c("Variable", "Date", "Value", "Units"),]
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
  check_columns(x, c("Variable", "Value", "Units"))
  x <- add_missing_columns(x, list("Date" = as.Date("2000-01-01")))

  check_by(by, colnames(x), res_names = unique(
    c("Variable", "Value", "Units", "Date"),
    colnames(wqbc::limits), colnames(wqbc::codes)))

  x <- delete_columns(x, colnames(x)[!colnames(x) %in% c("Variable", "Value", "Units", "Date", by)], messages = FALSE)

  check_class_columns(x, list("Variable" = c("character", "factor"),
                              "Value" = "numeric",
                              "Units" = c("character", "factor"),
                              "Date" = "Date"))

  x$Variable <- substitute_variables(x$Variable, messages = TRUE)
  x$Units <- substitute_units(x$Units, messages = TRUE)
  is.na(x$Variable[!x$Variable %in% get_variables()]) <- TRUE
  is.na(x$Units[!x$Units %in% get_units()]) <- TRUE
  x$Value <- replace_negative_values_with_na(x$Value)

  x <- delete_rows_with_missing_values(x)
 # check_rows(x)

  if(is.null(by))
    return(calc_limit(x))

  plyr::ddply(x, .variables = by, .fun = calc_limit)
}
