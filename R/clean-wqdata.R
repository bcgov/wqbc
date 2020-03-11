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

cv <- function(x) {
  if (length(x) == 1) {
    return(0)
  }
  if (mean(x) == 0) {
    return(0)
  }
  stats::sd(x) / mean(x)
}

abs_dev <- function(x) {
  abs(x - mean(x))
}

clean_wqdata_replicates <- function(x, max_cv, messages) {
  n <- nrow(x)
  cv <- cv(x$Value)
  if (cv(x$Value) > max_cv && nrow(x) > 2) {
    x <- dplyr::arrange_(x, ~ -Value)
    while (cv(x$Value) > max_cv && nrow(x) > 2) {
      x <- x[-which.max(abs_dev(x$Value)), ]
    }
  }
  x$Value <- mean(x$Value)

  if (!is.null(x$DetectionLimit)) {
    x$DetectionLimit <- mean(x$DetectionLimit)
  }

  if (messages && n > nrow(x)) {
    message(
      "Filtered ", n - nrow(x), " of ", n,
      " replicate values with a CV of ", signif(cv, 3), " for ", x$Variable[1],
      " on ", x$Date[1], "."
    )
  }
  x[1, , drop = FALSE]
}

clean_wqdata_variable <- function(x, max_cv, messages) {
  if (anyDuplicated(x$Date)) {
    x <- plyr::ddply(x, "Date", clean_wqdata_replicates,
      max_cv = max_cv,
      messages = messages
    )
  }
  x
}

clean_wqdata_by <- function(x, max_cv, messages) {
  if (anyDuplicated(x$Variable)) {
    x <- plyr::ddply(x, "Variable", clean_wqdata_variable,
      max_cv = max_cv,
      messages = messages
    )
  }
  x
}

#' Clean Water Quality Data
#'
#' Cleans water quality data. After standardization using \code{\link{standardize_wqdata}}
#' replicates (two or more readings for the same variable on the same date) are averaged
#' using the \code{mean} function.
#' Readings for the same variable on the same date but at different levels of the
#' columns specified in by are not considered replicates. The \code{clean_wqdata}
#' function is automatically called by \code{\link{calc_limits}} prior
#' to calculating limits.
#'
#' @details If there are three or more replicates with a coefficient of variation (CV) in
#' exceedance of \code{max_cv} then the replicates with the highest absolute deviation
#' is dropped until the CV is less than or equal to \code{max_cv}
#' or only two values remain. By default all values are averaged.
#'
#' A max_cv value of 1.29
#' is exceeded by two zero and one positive value (CV = 1.73)
#' or by two identical positive values and a third value an order
#' or magnitude greater (CV = 1.30). It is not exceed by one zero
#' and two identical positive values (CV = 0.87).
#'
#' @param x The data.frame to clean.
#' @param by A character vector of the columns in x to perform the cleaning by.
#' If you have multiple stations specify the column name that contains the station IDs.
#' @param max_cv A number indicating the maximum permitted coefficient
#' of variation for replicates.
#' @param sds The number of standard deviations above which a value is considered an outlier.
#' @param ignore_undetected A flag indicating whether to ignore undetected values when calculating the average deviation and identifying outliers.
#' @param large_only A flag indicating whether only large values which exceed the sds should be identified as outliers.
#' @param delete_outliers A flag indicating whether to delete outliers or merely flag them.
#' @param remove_blanks Should blanks be removed? Blanks are assumed to be denoted by
#' a value of `"Blank..."` in the `SAMPLE_CLASS` column. Default `FALSE`
#' @param messages A flag indicating whether to print messages.
#'
#' @examples
#' clean_wqdata(wqbc::dummy, messages = TRUE)
#' @seealso \code{\link{calc_limits}} and \code{\link{standardize_wqdata}}
#' @export
clean_wqdata <- function(x, by = NULL, max_cv = Inf,
                         sds = 10, ignore_undetected = TRUE,
                         large_only = TRUE, delete_outliers = FALSE,
                         remove_blanks = FALSE,
                         messages = getOption("wqbc.messages", default = TRUE)) {

  chk_data(x)
  chkor(chk_null(by), check_values(by, ""))
  chk_number(max_cv)
  check_values(messages, TRUE)

  chk_range(length(sds), c(1, 1))
  chk_range(sds, c(1, 100))
  chk_flag(ignore_undetected)
  chk_flag(large_only)
  chk_flag(delete_outliers)

  check_by(by, colnames(x), res_names = c("Value", "Outlier", "DetectionLimit"))

  x <- x[!is.na(x$Value), , drop = FALSE]

  if (remove_blanks) {
    if (!"SAMPLE_CLASS" %in% names(x)) {
      stop("SAMPLE_CLASS column must be present to remove blank records")
    }
    x <- x[!grepl("^[Bb]lank", x$SAMPLE_CLASS), ]
  }

  if (!tibble::has_name(x, "Date")) {
    if (tibble::has_name(x, "DateTime")) {
      if (messages) message("replacing DateTime column with Date")
      x$Date <- lubridate::date(x$DateTime)
      x$DateTime <- NULL
    } else {
      x <- add_missing_columns(x, list("Date" = as.Date("2000-01-01")), messages = messages)
    }
  }
  check_class_columns(x, list("Date" = "Date"))

  if ("DetectionLimit" %in% colnames(x)) {
    check_class_columns(x, list("DetectionLimit" = "numeric"))
  }

  # x <- standardize_wqdata(x, messages = messages)
  if (messages) message("Cleaning water quality data...")
  res <- c("Date", "Variable", "Code", "Value", "Units", "DetectionLimit", "ResultLetter")
  check_by(by, colnames(x), res_names = res)
  x <- del_cols_not_in_y(x, c(res, by))

  if (is.null(by)) {
    x <- clean_wqdata_by(x, max_cv = max_cv, messages = messages)
  } else {
    x <- plyr::ddply(x,
      .variables = by, .fun = clean_wqdata_by, max_cv = max_cv,
      messages = messages
    )
  }

  x %<>% identify_outliers(
    by = by, sds = sds, ignore_undetected = ignore_undetected, large_only = large_only,
    messages = messages
  )

  if (delete_outliers) {
    x %<>% dplyr::filter_(~ !is.na(Outlier) & !Outlier)
    if (messages) message("Deleted outliers.")
  }

  if (messages) message("Cleansed water quality data.")
  x
}
