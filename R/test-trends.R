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

# test for a sen trend
sen <- function(y, level = 0.95) {

  # set up output structure
  #cinames <- as.character(c((1-level)/2, 1 - (1-level)/2))
  cinames <- c("ci_lower", "ci_upper")
  out <- structure(rep(NA_real_, 3),
                   names = c("est", cinames))

  # if there are less than four years do not run test
  if (sum(!is.na(y)) < 4) {
    return(out)
  }

  # create time step variable and trim of NA (probably due to missing) values
  Year <- seq_along(y)[!is.na(y)]
  y <- y[!is.na(y)]

  # estimate sen slope
  ss <- zyp::zyp.sen(y ~ Year)
  # calculate confidence intervals
  ss.ci <- zyp::confint.zyp(ss, level = level)

  # fill in estimate and confidence interval for slope
  out["est"] <- stats::coef(ss)["Year"]
  out[cinames] <- ss.ci["Year",]

  # return
  out
}

do_test_trends <- function(data, breaks = NULL, FUN = "median", level = 0.95) {

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
  data %<>% apply(MARGIN = 2, sen, level = level) %>%
            t() %>% 
            tibble::as_data_frame() 
  data$months <- groups
  
  data
}



#' Thiel-Sen Trend Teste
#'
#' Analyses time series using the Thiel-Sen estimate of slope.
#'
#' The data must contain the columns Station, Date, Variable, Value, and Units.
#'
#' @param data The data.frame to analyse.
#' @param breaks A numeric vector used to create groups of consecutive months, if NULL the full
#'               year is used.
#' @param FUN The function to use for yearly summaries, e.g. median, mean, or max.
#' @param level The confidence level to compute a confidence interval for the Thiel-Sen slope.
#' @param messages A flag indicating whether to print messages.
#' @return A tibble data.frame with rows for each Station Variable, and month grouping, and
#'         additional columns for the slope estinate and confidence intervals.
#' @examples
#' #data <- standardize_wqdata(wqbc::dummy)
#' #test_trends(data, breaks = 6, messages = TRUE)
#' @export
test_trends <- function(data, breaks = NULL, FUN = "median", level = 0.95,
                        messages = getOption("wqbc.messages", default = TRUE)) {

  # check inputs
  check_flag(messages)
  check_cols(data, c("Station", "Date", "Variable", "Value", "Units"))
  check_data2(data, list(Date = Sys.Date(),
                         Value = c(1, NA)))

  # keep only relanvant columns
  data %<>% dplyr::select_(~Station, ~Date, ~Variable, ~Value, ~Units)

  # nest for analysis
  data %<>% tidyr::nest_("Data", c("Date", "Value"))

  # fit trends
  data %<>% dplyr::mutate_(Trend = ~purrr::map(Data, do_test_trends,
                                               breaks = breaks, FUN = FUN, level = level))

  # unnest and return
  data %>% tidyr::unnest_(unnest_cols = c("Trend"), .drop = TRUE)
}
