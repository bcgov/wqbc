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

categorize_wqi <- function (x) {
  cut(x, breaks = c(-1, 44, 64, 79, 94, 100),
      labels = c("Poor", "Marginal", "Fair", "Good", "Excellent"),
      ordered_result = TRUE)
}

add_failed <- function (x) {
  x$UpperLimit[is.na(x$UpperLimit)] <- Inf
  x$LowerLimit[is.na(x$LowerLimit)] <- -Inf
  x$Failed <- x$Value < x$LowerLimit | x$Value > x$UpperLimit
  x
}

calc_wqi <- function (x) {
  x <- add_failed(x)
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

  if(!all(c("Code", "Value", "UpperLimit") %in% colnames(x)))
    stop("x must contain columns Code, Value and UpperLimit")

  if(!"LowerLimit" %in% colnames(x)) {
    warning("x does not contain column LowerLimit")
    x$LowerLimit <- NA_real_
  }

  if(!is.numeric(x$Value)) stop("column Value must be numeric")
  if(!is.numeric(x$LowerLimit)) stop("column LowerLimit must be numeric")
  if(!is.numeric(x$UpperLimit)) stop("column UpperLimit must be numeric")

  if(any(is.na(x$Value))) {
    message("filtered ", length(is.na(x$Value)), " rows with missing values from x")
    x <- dplyr::filter_(x, ~!is.na(Value))
  }

  if(any(is.na(x$Code))) {
    message("filtered ", length(is.na(x$Code)), " rows with missing codes from x")
    x <- dplyr::filter_(x, ~!is.na(Code))
  }

  if(any(is.na(x$LowerLimit) & is.na(x$UpperLimit))) {
    message("filtered ", length(is.na(x$LowerLimit) & is.na(x$UpperLimit)), " rows without an upper or lower limit from x")
    x <- dplyr::filter_(x, ~!(is.na(LowerLimit) & is.na(UpperLimit)))
  }

  if(is.null(by)) {
    return(calc_wqi(x))
  }
  assert_that(is.character(by))

  if(!all(by %in% colnames(x)))
    stop("x must contain columns ", punctuate_strings(by, "and"), " in by")
  res_names <- c("Code", "Value", "UpperLimit", "LowerLimit")
  if(any(by %in% res_names))
     stop("by must not include ", punctuate_strings(res_names, "and"))

  plyr::ddply(x, .variables = by, .fun = calc_wqi)
}
