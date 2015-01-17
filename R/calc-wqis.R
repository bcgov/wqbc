# percent of failed variables
F1 <- function (x) {
  nfv <- length(unique(x$Variable[x$Failed]))
  nv <- length(unique(x$Variable))
  nfv / nv * 100
}

# percentage of failed tests
F2 <- function (x) {
  nft <- sum(x$Failed)
  nt <- nrow(x)
  nft / nt * 100
}

#' Is Within Limits
#'
#' @param value numeric vector of values to test
#' @param upper numeric vector of upper limits
#' @param lower numeric vector of lower limits
#' @return logical vector indicating whether within limits
#' @examples
#' library(dplyr)
#' data(ccme)
#' ccme$Within <- is_within_limits(ccme$Value, ccme$LowerLimit, ccme$UpperLimit)
#' filter(ccme, !Within)
#' @export
is_within_limits <- function (value, lower = NA_real_, upper = NA_real_) {
  get_excursion(value = value, lower = lower, upper = upper) == 0
}

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

  x <- data.frame(Value = value, LowerLimit = lower, UpperLimit = upper)

  x$LowerLimit[is.na(x$LowerLimit)] <- -Inf
  x$UpperLimit[is.na(x$UpperLimit)] <- Inf

  excursion <- rep(NA, nrow(x))

  print(x)

  for(i in 1:nrow(x)) {
    if(x$Value[i] >= x$LowerLimit[i]) {
      if(x$Value[i] <= x$UpperLimit[i]) {
        excursion[i] <- 0
      } else {
        excursion[i] <- x$Value[i] / x$UpperLimit[i] - 1
      }
    } else {
      excursion[i] <- x$LowerLimit[i] / x$Value[i] - 1
    }
  }
  excursion
}

# the amount by which failed test values do not meet their objectives
F3 <- function (x) {
  nt <- nrow(x)

  x <- x[x$Failed,,drop = FALSE]

  if(nrow(x) == 0)
    return (0)

  bol <- x$Value > x$UpperLimit
  excursion1 <- x$Value[bol] / x$UpperLimit[bol] - 1
  excursion2 <- x$LowerLimit[!bol] / x$Value[!bol] - 1

  nse <-  sum(excursion1, excursion2) / nt
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

calc_wqi <- function (x) {

  x$UpperLimit[is.na(x$UpperLimit)] <- Inf
  x$LowerLimit[is.na(x$LowerLimit)] <- -Inf
  x$Failed <- x$Value < x$LowerLimit | x$Value > x$UpperLimit

  F1 <- F1(x)
  F2 <- F2(x)
  F3 <- F3(x)
  WQI <- 100 - sqrt(F1^2 + F2^2 + F3^2) / 1.732
  Category <- categorize_wqi(WQI)
  data.frame(WQI = round(WQI), Category = Category,
             Variables = length(unique(x$Variable)), Tests = nrow(x),
             F1 = signif(F1, 3), F2 = signif(F2, 3), F3 = signif(F3, 3))
}

#' Calculate Water Quality Indices (WQIs)
#'
#' Calculates WQIs for x.
#'
#' @param x data.frame with Variable, Value, UpperLimit and if defined
#' LowerLimit columns
#' @param by character vector of columns to calculate WQIs by.
#' @param parallel flag indicating whether to calculate limits by the by argument using the parallel backend provided by foreach
#' @examples
#' data(ccme)
#' calc_wqis(ccme)
#' calc_wqis(ccme, by = "Date")
#'
#' @export
calc_wqis <- function (x, by = NULL,
                       parallel = getOption("wqbc.parallel", default = FALSE)) {
  assert_that(is.data.frame(x))
  assert_that(is.null(by) || (is.character(by) && noNA(by)))

  check_rows(x)
  check_columns(x, c("Variable", "Value", "UpperLimit"))
  x <- add_missing_columns(x, list("LowerLimit" = NA_real_))

  check_by(by, colnames(x), res_names = c("Variable", "Value", "LowerLimit", "UpperLimit"))

  x <- delete_columns(x, colnames(x)[!colnames(x) %in% c("Variable", "Value", "LowerLimit", "UpperLimit", by)], messages = FALSE)

  check_class_columns(x, list("Value" = "numeric",
                              "LowerLimit" = "numeric",
                              "UpperLimit" = "numeric"))

  x$Value <- replace_negative_values_with_na(x$Value)

  x <- delete_rows_with_missing_values(x, list("Value", "Variable",
                                               c("LowerLimit", "UpperLimit")))
  check_rows(x)

  if(is.null(by))
    return(calc_wqi(x))

  plyr::ddply(x, .variables = by, .fun = calc_wqi, .parallel = parallel)
}
