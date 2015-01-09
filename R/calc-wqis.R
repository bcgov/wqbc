# percent of failed variables
F1 <- function (x) {
  nfv <- length(unique(x$Code[x$Failed]))
  nv <- length(unique(x$Code))
  nfv / nv * 100
}

# percentage of failed tests
F2 <- function (x) {
  nft <- sum(x$Failed)
  nt <- nrow(x)
  nft / nt * 100
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
             Variables = length(unique(x$Code)), Tests = nrow(x),
             F1 = signif(F1, 3), F2 = signif(F2, 3), F3 = signif(F3, 3))
}

#' Calculate Water Quality Indices (WQIs)
#'
#' Calculates WQIs for x.
#'
#' @param x data.frame with Code, Value, UpperLimit and if defined
#' LowerLimit columns
#' @param by character vector of columns to calculate WQIs by.
#' @examples
#' data(ccme)
#' calc_wqis(ccme)
#' calc_wqis(ccme, by = "Date")
#'
#' @export
calc_wqis <- function (x, by = NULL) {
  assert_that(is.data.frame(x))
  assert_that(is.null(by) || (is.character(by) && noNA(by)))

  check_rows(x)
  check_columns(x, c("Code", "Value", "UpperLimit"))
  x <- add_missing_columns(x, list("LowerLimit" = NA_real_))

  check_class_columns(x, list("Value" = "numeric",
                              "LowerLimit" = "numeric",
                              "UpperLimit" = "numeric"))

  delete_rows_with_missing_values(x, list("Value", "Code",
                                          c("LowerLimit", "UpperLimit")))
  check_rows(x)

  check_by(by, x, res_names = c("Code", "Value", "LowerLimit", "UpperLimit"))

  if(is.null(by))
    return(calc_wqi(x))

  plyr::ddply(x, .variables = by, .fun = calc_wqi)
}
