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

# if there are only a two unique values then we can't really say which are outliers
adequate_unique_outliers <- function(x) {
  length(unique(x$Value[!x$Outlier])) > 2
}

identify_outliers_by <- function(x, sds, ignore_undetected, large_only, messages) {
  outlier <- x$Outlier
  missing <- is.na(x$Value)
  undetected <- !detected(x$Value, x$DetectionLimit)

  x$Outlier[missing] <- TRUE
  if (ignore_undetected) x$Outlier[undetected] <- TRUE

  if (adequate_unique_outliers(x)) {

    # only consider values which are not already outliers
    id_ok <- which(!x$Outlier)

    # calculate average deviation
    mdev <- stats::sd(x$Value[id_ok]) # because of adequate outliers sd(x) > 0

    scaled <- (x$Value[id_ok] - stats::median(x$Value[id_ok])) / mdev

    if (large_only) { # if large only then only only too large values are identified
      x$Outlier[id_ok] <- scaled > sds
    } else
      x$Outlier[id_ok] <- abs(scaled) > sds
  }

  x$Outlier[missing] <- FALSE
  if (ignore_undetected) x$Outlier[undetected] <- FALSE
  x$Outlier[outlier] <- TRUE

  x
}

#' Identify Outliers In Water Quality Data
#'
#' Identifies outliers in water quality data based on the standard deviation.
#'
#' @param data A data.frame with a numeric Value column to identify the outliers for.
#' @param by A character vector of the columns in x to perform the outlier detection by.
#' @param sds The number of standard deviations above which a value is considered an outlier.
#' @param ignore_undetected A flag indicating whether to ignore undetected values when calculating the average deviation and identifying outliers.
#' @param large_only A flag indicating wether only large values which exceed the sds should be identified as outliers.
#' @param messages A flag indicating whether to print messages.
#' @return The data with an additional logical column Outlier which indicates which values were identified as outliers.
#' @examples
#' data <- standardize_wqdata(wqbc::dummy)
#' identify_outliers(data, by = "Variable", messages = TRUE)
#' @seealso \code{\link{calc_limits}} and \code{\link{standardize_wqdata}}
#' @export
identify_outliers <- function(data, by = NULL, sds = 6, ignore_undetected = TRUE,
                              large_only = TRUE,
                              messages = getOption("wqbc.messages", default = TRUE)) {
  assert_that(is.null(by) || (is.character(by) && noNA(by)))
  check_scalar(sds, c(1, 100))
  check_flag(ignore_undetected)
  check_flag(large_only)
  check_flag(messages)

  check_by(by, colnames(data), res_names = c("Value", "Outlier", "DetectionLimit"))

  if (!tibble::has_name(data, "Outlier")) data$Outlier <- FALSE
  if (!tibble::has_name(data, "DetectionLimit")) data$DetectionLimit <- NA_real_

  check_data2(data, values = list(Value = c(1, NA),
                                  DetectionLimit = c(1, NA),
                                  Outlier = TRUE))

  if (is.null(by)) {
    data %<>% identify_outliers_by(sds = sds,
                                   ignore_undetected = ignore_undetected,
                                   large_only = large_only,
                                   messages = messages)
  } else {
    data %<>% plyr::ddply(.variables = by, .fun = identify_outliers_by,
                          sds = sds, ignore_undetected = ignore_undetected,
                          large_only = large_only,
                          messages = messages)
  }

  if (messages) message("Identified ", sum(data$Outlier[!is.na(data$Outlier)]), " outliers in water quality data.")
  data
}
