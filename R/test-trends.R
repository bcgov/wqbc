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

smk <- function(x) {
  x %<>% Kendall::SeasonalMannKendall()
  list(tau = x$tau, significance = x$sl)
}

do_test_trends <- function(data, use.median = TRUE) {

  data %<>% dplyr::mutate_(Month = ~lubridate::month(Date),
                         Year = ~lubridate::year(Date))

  years <- length(unique(data$Year))

  # if there are less than four years do not run test
  if (length(unique(data$Year)) < 4)
    return(list(tau = NA_real_, significance = NA_real_, years = years))

  data %<>% with(., tapply(Value, list(Year, Month), median)) %>% stats::as.ts()
  data %<>% smk()
  data$years <- years
  data
}

#' Seasonal Mann-Kendall Trend Teste
#'
#' Analyses time series using the Seasonal Mann-Kendall test.
#'
#' The data must contain the columns Station_Name, Date, Variable, Value, and Units.
#'
#' @param data The data.frame to analyse.
#' @param messages A flag indicating whether to print messages.
#' @return A tibble data.frame with The data with an additional logical column Outlier which indicates which values were identified as outliers.
#' @examples
#' data <- standardize_wqdata(wqbc::dummy)
#' identify_outliers(data, by = "Variable", messages = TRUE)
#' @seealso \code{\link{calc_limits}} and \code{\link{standardize_wqdata}}
#' @export
test_trends <- function(data, messages = getOption("wqbc.messages", default = TRUE)) {
  check_flag(messages)

  check_cols(data, c("Station", "Date", "Variable", "Value", "Units"))

  data %<>% dplyr::select_(~Station, ~Date, ~Variable, ~Value, ~Units)

  check_data2(data, list(Date = Sys.Date(),
                         Value = c(1, NA)))

  data %<>% tidyr::nest_("Data", c("Date", "Value"))

  data %<>% dplyr::mutate_(Trend = ~purrr::map(Data, do_test_trends))

  data %<>% dplyr::mutate_(Years = ~purrr::map_dbl(Trend, function(x) x$years),
                           Tau = ~purrr::map_dbl(Trend, function(x) x$tau),
                           Significance = ~purrr::map_dbl(Trend, function(x) x$significance))

  data %<>% tidyr::unnest_(unnest_cols = c("Tau", "Significance"), .drop = TRUE)

  data
}
