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

    new <- TRUE
    while (new && adequate_unique_outliers(x)) {
      id_ok <- which(!x$Outlier)
      # calculate average deviation
      mdev <- stats::sd(x$Value[id_ok]) # because of adequate outliers sd(x) > 0

      scaled <- (x$Value[id_ok] - stats::median(x$Value[id_ok])) / mdev

      if (large_only) { # if large only then only only too large values are identified
        bol <- scaled > sds
      } else {
        bol <- abs(scaled) > sds
      }

      x$Outlier[id_ok] <- bol
      new <- any(bol)
    }
  }
  x$Outlier[missing] <- FALSE
  if (ignore_undetected) x$Outlier[undetected] <- FALSE
  x$Outlier[outlier] <- TRUE

  x
}

identify_outliers <- function(data, by = NULL, sds = 6, ignore_undetected = TRUE,
                              large_only = TRUE,
                              messages = getOption("wqbc.messages", default = TRUE)) {
  if (!tibble::has_name(data, "Outlier")) data$Outlier <- FALSE
  if (!tibble::has_name(data, "DetectionLimit")) data$DetectionLimit <- NA_real_

  check_data(data, values = list(
    Value = c(1, NA),
    DetectionLimit = c(1, NA),
    Outlier = TRUE
  ))

  by <- c("Variable", by)

  if (is.null(by)) {
    data %<>% identify_outliers_by(
      sds = sds,
      ignore_undetected = ignore_undetected,
      large_only = large_only,
      messages = messages
    )
  } else {
    data %<>% plyr::ddply(
      .variables = by, .fun = identify_outliers_by,
      sds = sds, ignore_undetected = ignore_undetected,
      large_only = large_only,
      messages = messages
    )
  }

  if (messages) message("Identified ", sum(data$Outlier[!is.na(data$Outlier)]), " outliers in water quality data.")
  data
}
