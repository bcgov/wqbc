# Copyright 2015 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

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
#' Categorizes numbers between 1 and 100 into
#' the categories defined in the CCME Water Quality Index 1.0 User's Manual.
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

resample_wqi_tidy <- function (x, i, nt = nt, nv = nv) {
  x <- x[i,,drop = FALSE]
  wqif_excursion_variable(x = x$Excursion, v = x$Variable, nt = nt, nv = nv)["WQI"]
}

bootstrap_wqis_tidy <- function (x, R) {
  stopifnot(!any(is.na(x$Excursion)))

  nt <- nrow(x)
  nv <- length(unique(x$Variable))

  x <- x[x$Variable %in% x$Variable[x$Excursion != 0],,drop = FALSE]

  if(!nrow(x))
    return (c(100, 100))

  x$Variable <- factor(x$Variable)

  boot::boot(data = x, statistic = resample_wqi_tidy, R = R,
             strata = x$Variable, nt = nt, nv = nv)
}

lower_upper <- function (x) {
  round(quantile(x$t, c(0.025, 0.975)), 1)
}

boot_wqis <- function (x) {
  ci <- "row"
  cesi_code <- FALSE

  stopifnot(is.flag(cesi_code) & noNA(cesi_code))
  stopifnot(is.string(ci) & noNA(ci))
  stopifnot(ci %in% c("row", "column", "tidy"))

  R <- 10^3

  if(ci == "tidy") {
    boot <- bootstrap_wqis_tidy(x, R = R)
    return (lower_upper(boot))
  }

  if (cesi_code) {
    stop("cesi_code not available at this time")
    # x <- dplyr::mutate_(x, Excursion = ~Excursion + 1)
  }
  x <- dplyr::select_(x, ~Variable, ~Excursion, ~Date)
  x <- tidyr::spread_(x, "Variable", "Excursion")
  x <- dplyr::select_(x, ~-Date)
  x <- as.matrix(x)

  if(cesi_code) {
#     stopifnot(ci %in% c("row", "column"))
#     boot <- BootCICol(x, Column = (ci == "column"), Par = 4, CL = 0.95, R = R)
#     names(boot) <- NULL
  } else {
    if(ci == "row") {
      boot <- boot::boot(x, statistic = wqi_matrix, R = R)
    } else # ci == column
      boot <- bootstrap_wqis_column(x, R = R)
    boot <-  lower_upper(boot)
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

calc_wqi_by <- function (x, messages) {
  by <- colnames(x)
  by <- by[!by %in% c("Date", "Variable", "Value", "UpperLimit", "LowerLimit",
                          "DetectionLimit")]
  if(length(by))
    byc <- as.character(x[1,by,drop = FALSE])

  x$Excursion <- get_excursions(x$Value, x$LowerLimit, x$UpperLimit)
  check_excursions(x)
  x <- dplyr::select_(x, ~Excursion, ~Variable, ~Date)

  nt <- nrow(x)
  nv <- length(unique(x$Variable))
  wqi <- wqif_excursion_variable(x = x$Excursion, v = x$Variable, nt = nt, nv = nv)

  boot <- boot_wqis(x)

  wqi <- data.frame(WQI = round(wqi["WQI"], 1), Lower = boot[1], Upper = boot[2],
                    Category = categorize_wqi(wqi["WQI"]),
                    Variables = nv, Tests = nt,
                    F1 = round(wqi["F1"], 1), F2 = round(wqi["F2"], 1),
                    F3 = round(wqi["F3"], 1))

  if(!fourtimesfour(x)) {

    if(messages) {

      if(length(by)) {
        message("Dropped WQI with less than four variables sampled at least four times by ",
                punctuate_strings(paste(by, byc, sep = ": "), qualifier = "and"), ".")
      } else
        message("Dropped WQI with less than four variables sampled at least four times.")
    }
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

#' Calculate Water Quality Index (WQI)
#'
#' Calculates a water quality index for a series of variables, values with upper limits
#' and optionally lower limits using the method detailed in the
#' CCME Water Quality Index 1.0 User's Manual.
#'
#' @details The upper limits can be generated using the \code{\link{calc_limits}} function
#' or can be provided by the user. In fact if x lacks both upper and lower limits
#' then the \code{calc_limits} function is automatically called prior to
#' calculating the WQI. If values are zero and detection limits are provided
#' then the values are set to be the detection limits. This is important when
#' the variable has lower limits because otherwise the excursion will be infinity
#' and it will not be possible to calculate the WQI. In this case \code{calc_wqi}
#' throws an informative error. Finally it is important to note that in order
#' for the WQI to be calculated the data set must include four variables each
#' with non-missing values on at least four separate days. In addition
#' to calculating the WQI the function also generates
#' 95% Lower and Upper bootstrap confidence intervals about the WQI. The confidence
#' intervals are generated by first spreading the data into wide format where each
#' row represents one date and then resampling the rows with replacement to
#' generate 1,000 replicates. The confidence intervals are the 2.5% and 97.5%
#' percentiles of the replicate WQIs.
#'
#' @param x A data.frame to calculate the WQI for.
#' @param by An optional character vector of the columns in x to calculate the WQI by.
#' @param messages A flag indicating whether to print messages.
#' @examples
#' data(ccme)
#' calc_wqi(ccme, messages = TRUE)
#' calc_wqi(ccme, by = "Date", messages = TRUE)
#' @seealso \code{\link{calc_limits}} and \code{\link{wqbc}}
#' @export
calc_wqi <- function (x, by = NULL,
                      messages = getOption("wqbc.messages", default = TRUE)) {
  assert_that(is.data.frame(x))
  assert_that(is.null(by) || (is.character(by) && noNA(by)))
  assert_that(is.flag(messages) && noNA(messages))

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
    x <- calc_wqi_by(x, messages = messages)
  } else {
    x <- plyr::ddply(x, .variables = by, .fun = calc_wqi_by,
                     messages = messages)
  }
  if(messages) message("Calculated water quality indices.")
  x
}
