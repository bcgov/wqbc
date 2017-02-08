# Copyright 2017 Province of British Columbia
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

# test for a trend
trend <- function(y) {

  # set up output structure
  out <- dplyr::data_frame(
    slope = NA_real_,
    lower = NA_real_,
    upper = NA_real_,
    intercept = NA_real_,
    significance = NA)

  # if there are less than six years do not run test
  if (sum(!is.na(y)) <= 5) {
    return(out)
  }

  # create time step variable and trim of NA (probably due to missing) values
  Year <- seq_along(y)[!is.na(y)]
  y <- y[!is.na(y)]

  # estimate sen slope
  ss <- zyp::zyp.sen(y ~ Year)
  # calculate confidence intervals
  ss.ci <- zyp::confint.zyp(ss, level = 0.95)
  # fill in estimate and confidence interval for slope
  out[1] <- stats::coef(ss)["Year"]
  out[2:3] <- ss.ci["Year",]
  out[4] <- stats::coef(ss)["Intercept"]
  out[5] <- !(out[2] <= 0 & out[3] >= 0)
  out
}

do_test_trends <- function(data, breaks, FUN) {

  # Summarise data
  data %<>% do_summarise_for_trends(breaks, FUN, return_year = FALSE)

  data %<>% as.data.frame() #%>% tidyr::gather_("Month", "Value", gather_cols = colnames(data), na.rm = TRUE)

  data %<>% purrr::map(trend)

  groups <- names(data)
  data %<>% dplyr::bind_rows()
  data$Month <- groups

  data
}

#' Thiel-Sen Trend Test
#'
#' Analyses time series using the Thiel-Sen estimate of slope. It ret
#'
#' The data must contain the columns Station, Date, Variable, Value, and Units.
#'
#' @param data The data.frame to analyse.
#' @param breaks A numeric vector used to create groups of consecutive months, if NULL the full
#'               year is used.
#' @param FUN The function to use for yearly summaries, e.g. median, mean, or max.
#' @param messages A flag indicating whether to print messages.
#' @return A tibble data.frame with rows for each Station, Variable, and month grouping, and
#'         additional columns for the sen slope estinate, 95\% confidence intervals on the slope, intercept and significance at the 5\% level for the sen slope.
#' @seealso \code{\link[zyp]{zyp.sen}}
#' @references
#'
#' @examples
#'  data <- wqbc::yuepilon
#'  trend <- test_trends(data, breaks = 6, messages = TRUE)
#' \dontrun{
#'   demo(test_trends)
#' }
#' @export
test_trends <- function(data, breaks = NULL, FUN = "median", messages = getOption("wqbc.messages", default = TRUE)) {

  # check inputs
  check_flag(messages)

  check_cols(data, c("Station", "Date", "Variable", "Value", "Units"))
  check_data2(data, list(Date = Sys.Date(),
                         Value = c(1, NA)))

  # keep only relevant columns
  data %<>% dplyr::select_(~Station, ~Date, ~Variable, ~Value, ~Units)

  # nest for analysis
  data %<>% tidyr::nest_("Data", c("Date", "Value"))

  # fit trends
  data %<>% dplyr::mutate_(Trend = ~purrr::map(Data, do_test_trends,
                                               breaks = breaks, FUN = FUN))

  # unnest and return
  data %>% tidyr::unnest_(unnest_cols = c("Trend"), .drop = TRUE)
}

do_summarise_for_trends <- function(data, breaks, FUN, return_year = TRUE) {

  # get function to use for summarise multiple observations
  FUN <- match.fun(FUN)

  # if no breaks, use all months
  if (is.null(breaks)) breaks <- 12
  breaks <- c(0, breaks, 12) %>% sort() %>% unique()

  # add Month, Year and month grouping columns
  data %<>% dplyr::mutate_(Month = ~lubridate::month(Date),
                           Year  = ~lubridate::year(Date)) %>%
    dplyr::mutate_(group = ~cut(Month, breaks))

  # summarise by group
  data %<>% with(., tapply(Value, list(Year, group), FUN))

  # add year as column rather than as a rowname
  if (return_year) {
    data <- cbind.data.frame(Year = as.numeric(rownames(data)), data)
    rownames(data) <- NULL
  }

  data
}


#' Summarise data by year and month
#'
#' Compute annual summaries of water quality observations.
#'
#' The data must contain the columns Station, Date, Variable, Value, and Units.
#'
#' @param data The data.frame to analyse.
#' @param breaks A numeric vector used to create groups of consecutive months, if NULL the full
#'               year is used.
#' @param FUN The function to use for yearly summaries, e.g. median, mean, or max.
#' @param messages A flag indicating whether to print messages.
#'
#' @return A tibble data.frame with rows for each Station, Variable, Year and month grouping.
#'
#' @examples
#'  # select one station
#'  data(yuepilon)
#'  data <- yuepilon[yuepilon$Station == "02EA005", ]
#'  # estimate trend (using simple sen slope)
#'  trend <- test_trends(data, messages = TRUE)
#'  # get the data used in the test
#'  datasum <- summarise_for_trends(data)
#'  plot(datasum$Year, datasum$Value,
#'       main = paste("p-value =", round(trend$significance, 3)),
#'       ylab = "Value", xlab = "Year", las = 1)
#' @export
summarise_for_trends <- function(data, breaks = NULL, FUN = "median",
                                 messages = getOption("wqbc.messages", default = TRUE)) {

  # check inputs
  check_flag(messages)
  check_cols(data, c("Station", "Date", "Variable", "Value", "Units"))
  check_data2(data, list(Date = Sys.Date(),
                         Value = c(1, NA)))

  # keep only relevant columns
  data %<>% dplyr::select_(~Station, ~Date, ~Variable, ~Value, ~Units)

  # nest for analysis
  data %<>% tidyr::nest_("Data", c("Date", "Value"))

  # summarise
  data %<>% dplyr::mutate_(Summary = ~purrr::map(Data, do_summarise_for_trends,
                                                 breaks = breaks, FUN = FUN))

  # unnest
  data %<>% tidyr::unnest_(unnest_cols = c("Summary"), .drop = TRUE)

  # gather and return
  gather_cols <- setdiff(names(data), c("Station", "Variable", "Units", "Year"))
  data %>% tidyr::gather_("Month", "Value", gather_cols = gather_cols)
}
