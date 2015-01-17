join_codes <- function (x) {
  x <- dplyr::rename_(x, "..Units" = "Units")
  x <- dplyr::left_join(x, wqbc::codes, by = "Variable")
  stopifnot(!any(is.na(x$Units)))
  x$Value <- convert_units(x$Value, from = x$..Units, to = x$Units)
  x$..Units <- NULL
  x
}

average_daily_values_day_variable <- function (x) {
  txt <- paste0("x$Value <- ", x$Average, "(x$Value)")
  eval(parse(text = txt))
  x[1,,drop = FALSE]
}

average_daily_values_day <- function (x) {
  if(anyDuplicated(x$Variable))
    x <- plyr::ddply(x, "Variable", average_daily_values_day_variable)
  x
}

average_daily_values <- function (x) {
  if(anyDuplicated(dplyr::select_(x, ~Date, ~Variable)))
    x <- plyr::ddply(x, "Date", average_daily_values_day)
  x
}

average_monthly_values_variable <- function (x) {
  x$Weeks <- length(unique(lubridate::week(x$Date)))
  x$Values <- nrow(x)
  average_daily_values_day_variable(x)
}

average_monthly_values <- function (x) {
  x <- plyr::ddply(x, "Variable", average_monthly_values_variable)
  x
}

join_limits <- function (x) {
  x$..ID <- 1:nrow(x)
  x <- dplyr::left_join(x, wqbc::limits, by = c("Variable", "Units"))
  x
}

get_code_values <- function (x) {
  code_value <-  as.list(x$Value)
  names(code_value) <- x$Code
  code_value[!duplicated(names(code_value))]
}

test_condition <- function (x, cv) {
  if(is.na(x))
    return (TRUE)
  x <- try(eval(parse(text = x), envir = cv), silent = TRUE)
  if(class(x) != "logical")
    return (FALSE)
  return (x)
}

calc_limit <- function (x, cv) {
  x <- try(eval(parse(text = as.character(x)), envir = cv), silent = TRUE)
  if(class(x) != "numeric")
    return (NA)
  return (x)
}

calc_limits_by_period <- function (x) {
  cv <- get_code_values(x)
  x$Condition <- vapply(x$Condition, FUN = test_condition,
                        FUN.VALUE = logical(1), cv = cv)

  x <- x[x$Condition,,drop = FALSE]
  x$Condition <- NULL

  x$LowerLimit <- vapply(x$LowerLimit, FUN = calc_limit,
                         FUN.VALUE = numeric(1), cv = cv)

  x$UpperLimit <- vapply(x$UpperLimit, FUN = calc_limit,
                         FUN.VALUE = numeric(1), cv = cv)

  x[!is.na(x$LowerLimit) | !is.na(x$UpperLimit),,drop = FALSE]
}

calc_limits_by_day <- function (x) {
  x <- dplyr::filter_(x, ~Period == "Day")
  x <- plyr::ddply(x, "Date", calc_limits_by_period)

  stopifnot(!anyDuplicated(x$..ID))

  x <- dplyr::select_(x, ~-..ID, ~-Code, ~-Average)
  x
}

calc_limits_by_month <- function (x) {
  x <- dplyr::filter_(x, ~Period == "Month")
  x$Year <- lubridate::year(x$Date)
  x$Month <- lubridate::month(x$Date)

  x <- plyr::ddply(x, c("Year", "Month"), average_monthly_values)
  x <- plyr::ddply(x, c("Year", "Month"), calc_limits_by_period)
  x <- dplyr::filter_(x, ~Values >= 5 & Weeks >= 3)

  if(!nrow(x))
    return (x)

  x$Date <- as.Date(paste(x$Year, x$Month, "01", sep = "-"))

  stopifnot(!anyDuplicated(x$..ID))

  x <- dplyr::select_(x, ~-..ID, ~-Code, ~-Average, ~-Year, ~-Month, ~-Values, ~-Weeks)
  x
}

calc_limits_by <- function (x) {
  x <- join_codes(x)
  x <- average_daily_values(x)
  x <- join_limits(x)

  day <- calc_limits_by_day(x)
  month <- calc_limits_by_month(x)

  x <- rbind(day, month)
  x <- dplyr::select_(x, ~Period, ~Date, ~Variable, ~Value, ~LowerLimit, ~UpperLimit, ~Units)
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
#' @param parallel flag indicating whether to calculate limits by the by argument using the parallel backend provided by foreach
#' data(fraser)
#' fraser <- calc_limits(rbind(fraser[1:100,],fraser[1:100,]), message = FALSE)
#' @export
calc_limits <- function (x, by = NULL,
                         messages = getOption("wqbc.messages", default = TRUE),
                         parallel = getOption("wqbc.parallel", default = FALSE)) {
  assert_that(is.data.frame(x))
  assert_that(is.null(by) || (is.character(by) && noNA(by)))
  assert_that(is.flag(messages) && noNA(messages))
  assert_that(is.flag(parallel) && noNA(parallel))

  check_rows(x)
  check_columns(x, c("Variable", "Value", "Units"))
  x <- add_missing_columns(x, list("Date" = as.Date("2000-01-01")))

  check_by(by, colnames(x), res_names = unique(
    c("Variable", "Value", "Units", "Date",
      colnames(wqbc::limits), colnames(wqbc::codes))))

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

  plyr::ddply(x, .variables = by, .fun = calc_limits_by, .parallel = parallel)
}
