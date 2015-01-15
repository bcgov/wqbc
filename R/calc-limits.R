is_condition_satisfied <- function (x, condition) {
#  print(x)
 # print(condition)
  # need to get symbol(s)....and then filter down...
  TRUE
}

keep_conditions <- function (x) {
  if(any(!x$..Keep)) {
    wch <- which(!x$..Keep)
    for(i in wch) {
      x$..Keep[i] <- is_condition_satisfied(x, as.character(x$Condition[i]))
    }
  }
  x
}

calc_limits_month <- function (x) {
  x$Week <- lubridate::week(x$Date)
#  print(x)
#  stop()
x

}

calc_limits_date <- function (x) {
  x$..Keep <- is.na(x$Condition)
  x <- keep_conditions(x)
#  x$LowerLimit <- calc_limits_row(x, x$LowerLimit)
#  x$UpperLimit <- calc_limits_row(x, x$UpperLimit)
  x
}

calc_limits_by <- function (x) {
  x <- dplyr::rename_(x, "..Units" = "Units")
  x <- dplyr::left_join(x, wqbc::codes, by = "Variable")
  stopifnot(!any(is.na(x$Units)))
  x$Value <- convert_units(x$Value, from = x$..Units, to = x$Units)
  x$..Units <- NULL

  x$..ID <- 1:nrow(x)

  x <- dplyr::left_join(x, wqbc::limits, by = c("Variable", "Code", "Units"))

  x$..Keep <- FALSE

  max <- dplyr::filter_(x, ~is.na(Average))
  avg <- dplyr::filter_(x, ~!is.na(Average))

  avg$Year <- lubridate::year(avg$Date)
  avg$Month <- lubridate::month(avg$Date)

  max <- plyr::ddply(max, .variables = "Date", .fun = calc_limits_date)
  avg <- plyr::ddply(avg, .variables = c("Year", "Month"), .fun = calc_limits_month)

  # 1) determine in maximum and mean (keep both) if 5 measurements from 3 weeks in same month - actually calculate for everything by year and month....
  # Note pH should be median....refernce by conditions
  # 2) then see if conditions met (DROP if not)
  # 3) calculate limits
  #
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
#' @param messages flag indicating whether to print messages
#' @examples
#' data(fraser)
#' fraser <- calc_limits(fraser, message = FALSE)
#' @export
calc_limits <- function (x, by = NULL, messages = TRUE) {
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

  x$Variable <- substitute_variables(x$Variable, messages = messages)
  x$Units <- substitute_units(x$Units, messages = messages)
  is.na(x$Variable[!x$Variable %in% get_variables()]) <- TRUE
  is.na(x$Units[!x$Units %in% get_units()]) <- TRUE
  x$Value <- replace_negative_values_with_na(x$Value, messages = messages)

  x <- delete_rows_with_missing_values(x, messages = messages)
  check_rows(x)

  if(is.null(by))
    return(calc_limits_by(x))

  plyr::ddply(x, .variables = by, .fun = calc_limits_by)
}
