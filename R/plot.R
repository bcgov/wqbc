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

plot_timeseries_by <- function(data, title = NULL, y0, size, messages) {
  if (!is.null(title)) chk_string(title)

  data %<>% dplyr::mutate(Detected = detected(.data$Value, .data$DetectionLimit))

  data$Detected %<>% factor(levels = c(TRUE, FALSE))
  data$Outlier %<>% factor(levels = c(TRUE, FALSE))

  gp <- ggplot2::ggplot(data, ggplot2::aes_string(x = "Date", y = "Value"))

  if (!is.null(title)) gp <- gp + ggplot2::ggtitle(title)

  if (any(!is.na(data$Outlier))) {
    if (any(!is.na(data$Detected))) {
      gp <- gp + ggplot2::geom_point(ggplot2::aes_string(color = "Outlier", alpha = "Detected"), size = size)
    } else {
      gp <- gp + ggplot2::geom_point(ggplot2::aes_string(color = "Outlier"), size = size)
    }
  } else {
    if (any(!is.na(data$Detected))) {
      gp <- gp + ggplot2::geom_point(ggplot2::aes_string(alpha = "Detected"), size = size)
    } else {
      gp <- gp + ggplot2::geom_point(size = size)
    }
  }

  if (any(!is.na(data$Outlier))) {
    gp <- gp + ggplot2::scale_color_discrete(drop = FALSE)
  }

  if (any(!is.na(data$Detected))) {
    gp <- gp + ggplot2::scale_alpha_discrete(range = c(1, 1 / 3), drop = FALSE)
  }

  if (y0) gp <- gp + ggplot2::expand_limits(y = 0)
  gp
}

plot_timeseries_fun <- function(data, by, y0, size, messages) {
  title <- paste(data[by][1, ], collapse = " ")
  plot_timeseries_by(data, title = title, y0 = y0, size = size, messages = messages)
}

#' Plot Time Series Data
#'
#' If \code{by = NULL} plot_timeseries returns a ggplot object.
#' Otherwise it returns a list of ggplot objects.
#'
#' @param data A data frame of the data to plot.
#' @param by A character vector of the columns to plot the time series by.
#' @param y0 A flag indicating whether to expand the y-axis limits to include 0.
#' @param size A number of the point size.
#' @param messages A flag indicating whether to print messages.
#' @export
#' @examples
#' plot_timeseries(ccme[ccme$Variable == "As", ])
#' plot_timeseries(ccme, by = "Variable")
plot_timeseries <- function(data, by = NULL, y0 = TRUE, size = 1,
                            messages = getOption("wqbc.messages", default = TRUE)) {
  chkor(chk_null(by), check_values(by, ""))

  chk_flag(y0)
  chk_flag(messages)

  check_by(by, colnames(data))
  if (!tibble::has_name(data, "DetectionLimit")) data$DetectionLimit <- NA_real_
  if (!tibble::has_name(data, "Outlier")) data$Outlier <- NA

  check_class_columns(data, list(
    "Date" = "Date", "Value" = "numeric",
    "DetectionLimit" = "numeric",
    "Outlier" = "logical"
  ))

  if (is.null(by)) {
    data %<>% plot_timeseries_by(y0 = y0, size = size, messages = messages)
  } else {
    data %<>% plyr::dlply(.variables = by, .fun = plot_timeseries_fun, by = by, y0 = y0, size = size, messages = messages)
  }
  data
}
