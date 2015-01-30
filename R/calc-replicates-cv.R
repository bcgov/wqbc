
calc_cv_variable_date <- function (x, messages) {
  y <- NULL
  if(length(unique(x$Units)) != 1) {
    w <- getOption("warn")
    on.exit(options(warn = w))
    options(warn = 2)
    y <- try(convert_units(x$Value, from = x$Units, to = x$Units[1]), silent = TRUE)
    options(warn = w)
    if(is.error(y)) {
      warning("Unable to convert inconsistent units for ", x$Variable[1], " on ", x$Date[1])
    } else {
      x$Value <- y
      x$Units <- x$Units[1]
      if(messages)
        message("Converted inconsistent units for ", x$Variable[1], " on ", x$Date[1])
    }
  }
  if(is.error(y)) {
    is.na(x$CV) <- TRUE
  } else {
    x$CV <- round(sd(x$Value) / mean(x$Value), 2)
  }
  x
}

calc_cv_variable <- function (x, messages) {
  if(anyDuplicated(x$Date))
    x <- plyr::ddply(x, "Date", calc_cv_variable_date, messages)
  x
}

#' Calculates Replicates Coefficient of Variation
#'
#' Calculates the coefficient of variation for values for
#' the same variable collected on the same date. Useful for identifying dates
#' in which there was substantial variation between replicates.
#' If possible replicates with inconsistent units are converted to the same
#' units - otherwse
#'
#' @param x data.frame with columns Variable, Value, Units and Date.
#' @param messages flag indicating whether to print messages
#' @param parallel flag indicating whether to calculate cvs
#'  using the parallel backend provided by foreach
#' @examples
#'
#' data <- data.frame(Variable = "A Variable", Date = as.Date("2000-01-01"),
#'                    Value = c(20, 1, 2), Units = c("mg/L", "mg/L", "mg/L"))
#'
#' data <- rbind(data, data.frame(Variable = "Another Variable", Date = as.Date("2000-01-02"),
#'                    Value = c(1, 10^3), Units = c("mg/L", "ug/L")))
#'
#' data <- rbind(data, data.frame(Variable = "Another Variable", Date = as.Date("2000-01-04"),
#'                    Value = c(1, 10^3), Units = c("ug/L", "mg/L")))
#'
#' data <- rbind(data, data.frame(Variable = "Yet Another Variable", Date = as.Date("1977-05-25"),
#'                    Value = c(1, 10^3), Units = c("ug/L", "midichlorians")))
#'
#' data <- rbind(data, data.frame(Variable = "Kryptonite", Date = as.Date("1978-12-01"),
#'                    Value = 1, Units = "ug/L"))
#'
#' calc_replicates_cv(data, messages = TRUE)
#' @export
calc_replicates_cv <- function (x, messages = getOption("wqbc.messages", default = TRUE),
                                parallel = getOption("wqbc.parallel", default = FALSE)) {

  assert_that(is.data.frame(x))
  assert_that(is.flag(messages) && noNA(messages))
  assert_that(is.flag(parallel) && noNA(parallel))

  check_rows(x)
  check_columns(x, c("Variable", "Value", "Units", "Date"))

  check_class_columns(x, list("Variable" = c("character", "factor"),
                              "Value" = "numeric",
                              "Units" = c("character", "factor"),
                              "Date" = "Date"))
  x$CV <- 0

  plyr::ddply(x, .variables = "Variable", .fun = calc_cv_variable,
              .parallel = parallel, messages = messages)
}
