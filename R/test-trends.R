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

# test for a trend
trend <- function(y, method = c("yuepilon", "zhang", "sen")) {

  # set up output structure
  out <- structure(rep(NA_real_, 4),
                   names = c("estimate", "lower", "upper", "sig"))

  # if there are less than four years do not run test
  if (sum(!is.na(y)) < 4) {
    return(out)
  }

  # create time step variable and trim of NA (probably due to missing) values
  Year <- seq_along(y)[!is.na(y)]
  y <- y[!is.na(y)]

  if (method == "sen") {
    # estimate sen slope
    ss <- zyp::zyp.sen(y ~ Year)
    # calculate confidence intervals
    ss.ci <- zyp::confint.zyp(ss, level = 0.95)
    # fill in estimate and confidence interval for slope
    out[1] <- stats::coef(ss)["Year"]
    out[2:3] <- ss.ci["Year",]
    out[4] <- Kendall::Kendall(y, Year)$sl
  } else
  if (method %in% c("yuepilon", "zhang")) {
    zs <- zyp::zyp.trend.vector(y, x = Year, method = method,
                                conf.intervals = TRUE,
                                preserve.range.for.sig.test = TRUE)
    out[] <- zs[c("trend", "lbound", "ubound", "sig")]
  } else {
    stop("Unknown method :", method)
  }

  # return
  out
}


do_test_trends <- function(data, breaks, FUN, method) {

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

  # if there are less than four years do not run test
  # ...

  # fit trend test by month grouping and return
  groups <- colnames(data)
  data %<>% apply(MARGIN = 2, trend, method = method) %>%
            t() %>%
            tibble::as_data_frame()
  data$months <- groups

  data
}



#' Thiel-Sen Trend Test
#'
#' Analyses time series using the Thiel-Sen estimate of slope.
#'
#' The data must contain the columns Station, Date, Variable, Value, and Units.
#'
#' @param data The data.frame to analyse.
#' @param breaks A numeric vector used to create groups of consecutive months, if NULL the full
#'               year is used.
#' @param FUN The function to use for yearly summaries, e.g. median, mean, or max.
#' @param method The method to use.
#' @param messages A flag indicating whether to print messages.
#'
#' @return A tibble data.frame with rows for each Station Variable, and month grouping, and
#'         additional columns for the slope estinate and confidence intervals.
#' @details
#'
#' This routine computes a prewhitened nonlinear trend on a vector of data, using either
#' Zhang's (described in Wang and Swail, 2001) or Yue Pilon's (describe in Yue Pilon, 2002)
#' method of prewhitening and Sen's slope, and use a Kendall test for significance.
#'
#' @examples
#'  data <- wqbc::yuepilon
#'  trend <- test_trends(data, breaks = 6, messages = TRUE)
#'  trend <- test_trends(data, breaks = 6, messages = TRUE, method = "sen")
#' @export
test_trends <- function(data, breaks = NULL, FUN = "median", method = "yuepilon",
                        messages = getOption("wqbc.messages", default = TRUE)) {

  # check inputs
  check_flag(messages)
  match.arg(method, c("yuepilon", "zhang", "sen"))
  check_cols(data, c("Station", "Date", "Variable", "Value", "Units"))
  check_data2(data, list(Date = Sys.Date(),
                         Value = c(1, NA)))

  # keep only relanvant columns
  data %<>% dplyr::select_(~Station, ~Date, ~Variable, ~Value, ~Units)

  # nest for analysis
  data %<>% tidyr::nest_("Data", c("Date", "Value"))

  # fit trends
  data %<>% dplyr::mutate_(Trend = ~purrr::map(Data, do_test_trends,
                                               breaks = breaks, FUN = FUN, method = method))

  # unnest and return
  data %>% tidyr::unnest_(unnest_cols = c("Trend"), .drop = TRUE)
}
