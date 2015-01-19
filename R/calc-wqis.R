#' Get Excursion
#'
#' @param value numeric vector of values to calculate excursion
#' @param upper numeric vector of upper limits
#' @param lower numeric vector of lower limits
#' @return numeric vector of excursions
#' @examples
#' library(dplyr)
#' data(ccme)
#' ccme$Excursion <- get_excursion(ccme$Value, ccme$LowerLimit, ccme$UpperLimit)
#' dplyr::filter(ccme, Excursion != 0)
#' @export
get_excursion <- function (value, lower = NA_real_, upper = NA_real_) {
  assert_that(is.numeric(value))
  assert_that(is.numeric(lower))
  assert_that(is.numeric(upper))

  less <- !is.na(lower) & value <= lower
  more <- !is.na(upper) & value >= upper

  if(any(less & more))
    stop("lower must be less than upper.")

  excursion <- rep(0, length(value))
  excursion[more] <- value[more] / upper[more] - 1
  excursion[less] <- lower[less] / value[less] - 1
  excursion
}

# percent of failed variables
F1 <- function (excursions, variables) {
  nfv <- length(unique(variables[excursions != 0]))
  nv <- length(unique(variables))
  nfv / nv * 100
}

# percentage of failed tests
F2 <- function (excursions) {
  nft <- sum(excursions != 0)
  nt <- length(excursions)
  nft / nt * 100
}

# the amount by which failed test values do not meet their objectives
F3 <- function (excursions) {
  nt <- length(excursions)

  nse <-  sum(excursions) / nt
  nse / (0.01 * nse + 0.01)
}

#' Categorize WQI Values
#'
#' @param x numeric vector of WQI values
#' @return factor of WQI categories
#' @examples
#' categorize_wqi(seq(0, 100, by = 5))
#' @export
categorize_wqi <- function (x) {
  assert_that(is.numeric(x))

  labels <- c("Poor", "Marginal", "Fair", "Good", "Excellent")
  x <- cut(x, breaks = c(-1, 44, 64, 79, 94, 100),
           labels = labels, ordered_result = TRUE)
  x <- factor(as.character(x), levels = rev(labels))
  x
}

wqi <- function (x) {

  Variables = length(unique(x$Variable))
  F1 <- F1(x$Excursion, x$Variable)
  F2 <- F2(x$Excursion)
  F3 <- F3(x$Excursion)
  WQI <- 100 - sqrt(F1^2 + F2^2 + F3^2) / 1.732
  return(c(WQI = WQI, Variables = Variables, Tests = nrow(x),
           F1 = F1, F2 = F2, F3 = F3))
}

resample_variable <- function (x) {
  x$Excursion <- sample(x$Excursion, size = length(x$Excursion), replace = TRUE)
  x
}

resample_wqi <- function (x) {
  x <- plyr::ddply(x, "Variable", resample_variable)
  wqi(x)["WQI"]
}

calc_wqi <- function (x, ci) {

  x$Excursion <- get_excursion(x$Value, x$LowerLimit, x$UpperLimit)

  x <- x[c("Date", "Variable", "Excursion")]

  wqi <- wqi(x)

  Lower <- NA
  Upper <- NA

  if(ci) {
    wqis <- rep(NA, 1000)
    wqis[1] <- wqi["WQI"]
    for(i in 2:1000) {
      wqis[i] <- resample_wqi(x)
    }
    qs <- quantile(wqis, c(0.025, 0.975))
    Lower <- round(qs[1])
    Upper <- round(qs[2])
  }

  Category <- categorize_wqi(wqi["WQI"])
  data.frame(WQI = round(wqi["WQI"]), Lower = Lower, Upper = Upper,
             Category = categorize_wqi(wqi["WQI"]),
             Variables = wqi["Variables"], Tests = wqi["Tests"],
             F1 = signif(wqi["F1"], 3), F2 = signif(wqi["F2"], 3),
             F3 = signif(wqi["F3"], 3))
}

#' Calculate Water Quality Indices (WQIs)
#'
#' Calculates WQIs for x.
#'
#' @param x data.frame with Variable, Value, UpperLimit and if defined
#' LowerLimit columns
#' @param by character vector of columns to calculate WQIs by
#' @param ci flag indicating whether to generate bootstrap
#' 95% confidence intervals using Method 1 of
#' Environmental Indicators: Their Development and Application.
#' @param messages flag indicating whether to print messages
#' @param parallel flag indicating whether to calculate limits by the by argument using the parallel backend provided by foreach
#' @examples
#' data(ccme)
#' calc_wqis(ccme)
#' calc_wqis(ccme, by = "Date")
#'
#' @export
calc_wqis <- function (x, by = NULL, ci = FALSE,
                       messages = getOption("wqbc.messages", default = TRUE),
                       parallel = getOption("wqbc.parallel", default = FALSE)) {
  assert_that(is.data.frame(x))
  assert_that(is.null(by) || (is.character(by) && noNA(by)))
  assert_that(is.flag(ci) && noNA(ci))

  check_rows(x)
  check_columns(x, c("Variable", "Value", "UpperLimit"))

  x <- add_missing_columns(x, list("Date" = as.Date("2000-01-01"),
                                   "LowerLimit" = NA_real_), messages = messages)

  check_by(by, colnames(x), res_names = c("Variable", "Value", "LowerLimit", "UpperLimit"))

  x <- delete_columns(x, colnames(x)[!colnames(x) %in% c("Date", "Variable", "Value", "LowerLimit", "UpperLimit", by)], messages = FALSE)

  check_class_columns(x, list("Date" = "Date",
                              "Value" = "numeric",
                              "LowerLimit" = "numeric",
                              "UpperLimit" = "numeric"))

  x$Value <- replace_negative_values_with_na(x$Value, messages = messages)

  x <- delete_rows_with_missing_values(x, list("Date", "Value", "Variable",
                                               c("LowerLimit", "UpperLimit")),
                                       messages = messages)
  check_rows(x)

  if(is.null(by))
    return(calc_wqi(x, ci = ci))

  plyr::ddply(x, .variables = by, ci = ci, .fun = calc_wqi, .parallel = parallel)
}
