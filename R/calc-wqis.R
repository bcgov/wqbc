get_excursions <- function (value, lower = NA_real_, upper = NA_real_) {
  assert_that(is.numeric(value))
  assert_that(is.numeric(lower))
  assert_that(is.numeric(upper))

  less <- !is.na(lower) & value <= lower
  more <- !is.na(upper) & value >= upper

  if(any(less & more))
    stop("the lower limit must be less than the upper limit")

  excursion <- rep(0, length(value))
  excursion[more] <- value[more] / upper[more] - 1
  excursion[less] <- lower[less] / value[less] - 1
  excursion
}

#' Categorize Water Quality Indices
#'
#' Categorizes WQI values between 1 and 100 into
#' the CCME categories.
#' @param x A numeric vector of the WQI values to categorize.
#' @examples
#' categorize_wqi(seq(1,100,by = 5))
#' @export
categorize_wqi <- function (x) {
  assert_that(is.numeric(x))

  labels <- c("Poor", "Marginal", "Fair", "Good", "Excellent")
  x <- cut(x, breaks = c(-1, 44, 64, 79, 94, 100),
           labels = labels, ordered_result = TRUE)
  x <- factor(as.character(x), levels = rev(labels))
  x
}

wqif <- function (nv, nt, nfv, nft, nse) {
  F1 <- nfv / nv * 100
  F2 <- nft / nt * 100
  F3 <- nse / (nse + 1) * 100

  WQI <- 100 - sqrt(F1^2 + F2^2 + F3^2) / 1.732
  c(WQI = WQI, F1 = F1, F2 = F2, F3 = F3)
}

wqif_excursion_variable <- function (x, v, nv, nt) {
  nft <- sum(x != 0)
  nfv <- length(unique(v[x != 0]))
  nse <-  sum(x) / nt

  wqif(nv = nv, nt = nt, nfv = nfv, nft = nft, nse = nse)
}

wqif_matrix <- function (x) {
  not_missing <- !is.na(x)
  nt <- sum(not_missing)
  not_all_missing <- apply(x, MARGIN = 2, FUN = function (x) !all(is.na(x)))
  nv <- sum(not_all_missing)
  nft <- sum(not_missing & x != 0)
  nfv <- sum(apply(x[,not_all_missing,drop = FALSE], MARGIN = 2,
                   FUN = function (x) any(!is.na(x) & x != 0)))
  nse <-  sum(x[not_missing]) / nt
  wqif(nv = nv, nt = nt, nfv = nfv, nft = nft, nse = nse)
}

wqi_matrix <- function (x, i = 1:nrow(x)) {
  wqif_matrix(x[i,,drop = FALSE])["WQI"]
}

resample_wqi_column <- function (x, i) {
  x <- x[i,,drop = FALSE]
  x <- x[!is.na(x$Excursion),]
  nt <- nrow(x)
  nv <- length(unique(x$Variable))
  wqif_excursion_variable(x = x$Excursion, v = x$Variable, nt = nt, nv = nv)["WQI"]
}

bootstrap_wqis_column <- function (x, R) {
  x <- as.data.frame(x)
  x <- tidyr::gather_(x, "Variable", "Excursion", colnames(x))
  boot::boot(data = x, statistic = resample_wqi_column, R = R, strata = x$Variable)
}

boot_wqis <- function (x, ci, cesi_code) {
  x <- dplyr::select_(x, ~Variable, ~Excursion, ~Date)
  x <- tidyr::spread_(x, "Variable", "Excursion")
  x <- dplyr::select_(x, ~-Date)
  x <- as.matrix(x)

  R <- 10^3

  if(cesi_code) {
    boot <- BootCICol(x, Column = (ci == "column"), Par = 4, CL = 0.95, R = R)
    names(boot) <- NULL
  } else {

    if(ci == "row") {
      boot <- boot::boot(x, statistic = wqi_matrix, R = R)
    } else {
      boot <- bootstrap_wqis_column(x, R = R)
    }
    boot <-  round(quantile(boot$t, c(0.025, 0.975)),1)
  }
  boot
}

four <- function (x) {
  data.frame(Four = nrow(x) >= 4)
}

fourtimesfour <- function (x) {
  x <- plyr::ddply(x, .variables = "Variable", .fun = four)
  x <- x[x$Four,,drop = FALSE]
  nrow(x) >= 4
}

calc_wqi <- function (x, ci, cesi_code, messages) {

  x$Excursion <- get_excursions(x$Value, x$LowerLimit, x$UpperLimit)
  check_excursions(x)
  x <- dplyr::select_(x, ~Excursion, ~Variable, ~Date)

  nt <- nrow(x)
  nv <- length(unique(x$Variable))
  wqi <- wqif_excursion_variable(x = x$Excursion, v = x$Variable, nt = nt, nv = nv)

  if(ci == "none") {
    boot <- rep(NA_real_,2)
  } else {
    boot <- boot_wqis(x, ci = ci, cesi_code = cesi_code)
  }

  wqi <- data.frame(WQI = round(wqi["WQI"], 1), Lower = boot[1], Upper = boot[2],
                    Category = categorize_wqi(wqi["WQI"]),
                    Variables = nv, Tests = nt,
                    F1 = round(wqi["F1"], 1), F2 = round(wqi["F2"], 1),
                    F3 = round(wqi["F3"], 1))

  if(!fourtimesfour(x)) {
    if(messages) message("Dropped WQI with less than four variables sampled at least four times.")
    wqi <- wqi[F,,drop = FALSE]
  }
  wqi
}

set_detection_limits <- function (x, messages) {
  bol <- x$Value == 0 & !is.na(x$DetectionLimit) & x$DetectionLimit > 0
  if(any(bol)) {
    x$Value[bol] <- x$DetectionLimit[bol]
    if(messages) message("Replaced ", sum(bol) ," of the ",
                         plural("value", sum(bol) > 1, " "),
                         "in column Value with the detection limit in column DetectionLimit.")
  }
  x
}

#' Calculate Water Quality Indices (WQIs)
#'
#' Calculates water quality indices.
#'
#' @param x The data.frame to perform the calculations on.
#' @param by A character vector of the columns to perform the calculations by.
#' @param ci A string indicating whether to calculate bootstrap confidence
#' intervals by "row" or "column" or "none".
#' @param cesi_code A flag indicating whether to use wqbc or cesi code to calculate the
#' confidence intervals.
#' @param messages A flag indicating whether to print messages.
#' @examples
#' data(ccme)
#' calc_wqis(ccme)
#' calc_wqis(ccme, by = "Date")
#' @export
calc_wqis <- function (x, by = NULL, ci = "row", cesi_code = FALSE,
                       messages = getOption("wqbc.messages", default = TRUE)) {
  assert_that(is.data.frame(x))
  assert_that(is.null(by) || (is.character(by) && noNA(by)))
  assert_that(is.flag(messages) && noNA(messages))
  assert_that(is.string(ci))

  ci <- tolower(ci)
  if(!ci %in% c("row", "column", "none")) stop("ci must be \"row\", \"column\", or \"none\"")

  check_rows(x)
  if(!any(c("LowerLimit", "UpperLimit") %in% colnames(x)))
    x <- calc_limits(x, by = by, messages = messages)

  check_columns(x, c("Variable", "Value", "UpperLimit"))

  if(messages) message("Calculating water quality indices...")

  x <- add_missing_columns(x, list(
    "Date" = as.Date("2000-01-01"), "LowerLimit" = NA_real_, "DetectionLimit" = 0), messages = messages)

  check_class_columns(x, list("Date" = "Date",
                              "Variable" = c("character","factor"),
                              "Value" = "numeric",
                              "LowerLimit" = "numeric",
                              "UpperLimit" = "numeric",
                              "DetectionLimit" = "numeric"))

  res <- c("Date", "Variable", "Value", "LowerLimit", "UpperLimit",
           "DetectionLimit")

  x$Variable <- as.character(x$Variable)

  check_by(by, colnames(x), res_names = res[res != "Date"])

  x <- del_cols_not_in_y(x, c(res, by))
  x <- delete_rows_with_certain_values(x, list("Date", "Variable"),
                                       messages = messages, txt = "missing")

  x <- delete_rows_with_certain_values(
    x, list("Value", c("LowerLimit", "UpperLimit")),
    messages = messages, txt = c("missing or negative"))

  x <- delete_rows_with_certain_values(x, list("UpperLimit"),
                                       messages = messages, txt = c("zero"))

  x <- set_detection_limits(x, messages = messages)

  check_rows(x)

  if(is.null(by)) {
    x <- calc_wqi(x, ci = ci, cesi_code = cesi_code, messages = messages)
  } else {
    x <- plyr::ddply(x, .variables = by, .fun = calc_wqi, ci = ci, cesi_code = cesi_code,
                     messages = messages)
  }
  if(messages) message("Calculated water quality indices.")
  x
}
