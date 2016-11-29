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

identify_outliers_by <- function(x, threshold, large_only, messages) {
  if (!adequate_unique_outliers(x)) return(x)

  # only consider values which are not already outliers
  id_ok <- which(!x$Outlier)

  # calculate average deviation
  mdev <- stats::mad(x$Value[id_ok])
  if (mdev == 0) mdev <- stats::sd(x$Value[id_ok]) # because of adequate outliers sd(x) > 0

  scaled <- (x$Value - stats::median(x$Value[id_ok])) / mdev

  if (large_only) { # if large only then only only too large values are identified
    x$Outlier[id_ok] <- scaled[id_ok] > threshold
  } else
    x$Outlier[id_ok] <- abs(scaled[id_ok]) > threshold
  x
}

#' Identify Outliers In Water Quality Data
#'
#' Identifies outliers in water quality data based on the average deviation.
#' If possible it uses the Mean Absolute Deviation (MAD)
#' but if the MAD is zero then it uses the Standard Deviation.
#'
#' @param data A data.frame with a numeric Value column to identify the outliers for.
#' @param by A character vector of the columns in x to perform the outlier detection by.
#' @param threshold A number indicating the number of average deviations above which a value is considered an outlier.
#' @param ignore_zeros A flag indicating whether to ignore zeros when calculating the average deviation and identifying outliers.
#' @param large_only A flag indicating wether only large values which exceed the threshold should be identified as outliers.
#' @param messages A flag indicating whether to print messages.
#' @return The data with an additional logical column Outlier which indicates which values were identified as outliers.
#' @examples
#' data <- standardize_wqdata(wqbc::dummy)
#' identify_outliers(data, by = "Variable", messages = TRUE)
#' @seealso \code{\link{calc_limits}} and \code{\link{standardize_wqdata}}
#' @export
identify_outliers <- function(data, by = NULL, threshold = 10, ignore_zeros = TRUE,
                              large_only = TRUE,
                            messages = getOption("wqbc.messages", default = TRUE)) {
  assert_that(is.null(by) || (is.character(by) && noNA(by)))
  check_scalar(threshold, c(1, 1000))
  check_flag(ignore_zeros)
  check_flag(large_only)
  check_flag(messages)

  check_cols(data, by)
  check_data2(data, values = list(Value = c(1, NA)))

  if (tibble::has_name(data, "Outlier")) error("data already has Outlier column")

  data$Outlier <- FALSE
  data$Outlier[is.na(data$Value)] <- TRUE
  if (ignore_zeros) data$Outlier[!is.na(data$Value) & data$Value == 0] <- TRUE

  if (is.null(by)) {
    data %<>% identify_outliers_by(threshold = threshold, large_only = large_only,
                                   messages = messages)
  } else {
    data %<>% plyr::ddply(.variables = by, .fun = identify_outliers_by,
                     threshold = threshold, large_only = large_only,
                            messages = messages)
  }

  if (ignore_zeros) is.na(data$Outlier[!is.na(data$Value) & data$Value == 0]) <- TRUE
  is.na(data$Outlier[is.na(data$Value)]) <- TRUE

  if (messages) message("Identified ", sum(data$Outlier[!is.na(data$Outlier)]), " outliers in water quality data.")
  data
}
