get_excursions <- function (value, lower = NA_real_, upper = NA_real_) {
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

categorize_wqi <- function (x) {
  assert_that(is.numeric(x))

  labels <- c("Poor", "Marginal", "Fair", "Good", "Excellent")
  x <- cut(x, breaks = c(-1, 44, 64, 79, 94, 100),
           labels = labels, ordered_result = TRUE)
  x <- factor(as.character(x), levels = rev(labels))
  x
}

wqi <- function (x, v, nv, nt) {
  nft <- sum(x != 0)
  nfv <- length(unique(v[x != 0]))
  nse <-  sum(x) / nt

  F1 <- nfv / nv * 100
  F2 <- nft / nt * 100
  F3 <- nse / (0.01 * nse + 0.01)

  WQI <- 100 - sqrt(F1^2 + F2^2 + F3^2) / 1.732
  return(c(WQI = WQI, F1 = F1, F2 = F2, F3 = F3))
}

resample_wqi <- function (x, i, nt = nt, nv = nv) {
  x <- x[i,,drop = FALSE]
  wqi <- wqi(x = x$Excursion, v = x$Variable, nt = nt, nv = nv)["WQI"]
  stopifnot(!is.na(wqi) && !is.nan(wqi))
  wqi(x = x$Excursion, v = x$Variable, nt = nt, nv = nv)["WQI"]
}

bootstrap_wqi <- function (x, nt, nv) {
  x <- x[x$Variable %in% x$Variable[x$Excursion != 0],,drop = FALSE]

  if(!nrow(x))
    return (c(100, 100))

  x$Variable <- factor(as.character(x$Variable))

  boot <- boot::boot(data = x, statistic = resample_wqi, R = 1000,
                     stype = "i", strata = x$Variable, nt = nt, nv = nv)

  quantile(boot$t, c(0.025, 0.975))
}

calc_wqi <- function (x) {

  x$Excursion <- get_excursions(x$Value, x$LowerLimit, x$UpperLimit)
  x <- dplyr::select_(x, ~Excursion, ~Variable)

  nt <- nrow(x)
  nv <- length(unique(x$Variable))

  wqi <- wqi(x = x$Excursion, v = x$Variable, nt = nt, nv = nv)

  limits <- bootstrap_wqi(x, nt = nt, nv = nv)
  Lower <- round(limits[1])
  Upper <- round(limits[2])

  data.frame(WQI = round(wqi["WQI"]), Lower = Lower, Upper = Upper,
             Category = categorize_wqi(wqi["WQI"]),
             Variables = nv, Tests = nt,
             F1 = signif(wqi["F1"], 3), F2 = signif(wqi["F2"], 3),
             F3 = signif(wqi["F3"], 3))
}

#' Calculate Water Quality Indices (WQIs)
#'
#' Calculates water quality indices.
#'
#' @param x The data.frame to perform the calculations on.
#' @param by A character vector of the columns to perform the calculations by.
#' @param messages A flag indicating whether to print messages.
#' @param parallel A flag indicating whether to use the parallel backend provided by foreach.
#' @examples
#' data(ccme)
#' calc_wqis(ccme)
#' calc_wqis(ccme, by = "Date")
#' @export
calc_wqis <- function (x, by = NULL,
                       messages = getOption("wqbc.messages", default = TRUE),
                       parallel = getOption("wqbc.parallel", default = FALSE)) {
  assert_that(is.data.frame(x))
  assert_that(is.null(by) || (is.character(by) && noNA(by)))

  check_rows(x)

  if(!all(c("LowerLimit", "UpperLimit") %in% colnames(x)))
    x <- calc_limits(x, by = by, messages = messages, parallel = parallel)

  check_columns(x, c("Variable", "Value", "UpperLimit"))

  if(messages) message("Calculating Water Quality Indices...")

  x <- add_missing_columns(x, list("Date" = as.Date("2000-01-01"),
                                   "LowerLimit" = NA_real_), messages = messages)
  check_class_columns(x, list("Date" = "Date",
                              "Variable" = c("character","factor"),
                              "Value" = "numeric",
                              "LowerLimit" = "numeric",
                              "UpperLimit" = "numeric"))

  check_by(by, colnames(x), res_names = c("Variable", "Value", "LowerLimit", "UpperLimit"))

  x <- delete_columns(x, colnames(x)[!colnames(x) %in% c("Date", "Variable", "Value", "LowerLimit", "UpperLimit", by)], messages = FALSE)

  x$Value <- replace_negative_values_with_na(x$Value, messages = messages)
  x$LowerLimit <- replace_negative_values_with_na(x$LowerLimit, messages = messages)
  x$UpperLimit <- replace_negative_values_with_na(x$UpperLimit, zero = TRUE, messages = messages)
  x <- delete_rows_with_missing_values(x, list("Date", "Value", "Variable",
                                               c("LowerLimit", "UpperLimit")),
                                       messages = messages)
  check_rows(x)

  if(is.null(by)) {
    x <- calc_wqi(x)
  } else {
    x <- plyr::ddply(x, .variables = by, .fun = calc_wqi, .parallel = parallel)
  }
  if(messages) message("Calculated.")
  x
}
