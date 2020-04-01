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


estimate_variable_values_by <- function(x, messages) {

  # if there is not enough observations, then
  # skip modelling and return with NAs
  # here defined as 6 x the year range
  ndata_years <- x %>%
    dplyr::filter(!is.na(.data$Value)) %>%
    dplyr::mutate(year = lubridate::year(.data$Date)) %>%
    dplyr::group_by(.data$year) %>%
    dplyr::tally()
  ndata_years <- sum(ndata_years$n >= 12)
  if (ndata_years == 0) {
    if (messages) message("Insufficient observations to allow modelling, using mean.")
    x$Value <- mean(x$Value, na.rm = TRUE)
  } else {
    # extract seasonal covariates from 'Date'
    x %<>% dplyr::mutate(
      yday = lubridate::yday(.data$Date), # for seasonal trend
      day = lubridate::decimal_date(.data$Date)
    ) # for long term trends

    if (ndata_years == 1) {
      # simply fit a seasonal smoother
      mod <- mgcv::gam(Value ~ s(yday, k = 6, bs = "cc"), data = x)

      # replace Values with modelled ones
      x$Value <- mgcv::predict.gam(mod, newdata = x)
    }
    if (ndata_years == 2) {
      # simply fit a seasonal smoother with a trend
      mod <- mgcv::gam(Value ~ s(day, k = 3) + s(yday, k = 6, bs = "cc"), data = x)

      # replace Values with modelled ones
      x$Value <- mgcv::predict.gam(mod, newdata = x)
    }
    if (ndata_years > 2) {
      # fit long term trend and seasonally evolving trend in 2 steps
      # step 1: long term trend.
      k_t <- ndata_years
      trend_f <- sprintf("Value ~ s(day, k = %i) + s(yday, bs = 'cc', k = 6)", as.integer(k_t))
      mod1 <- mgcv::gam(stats::formula(trend_f), data = x)
      ## set yday coeffs to zero to remove them from the model
      mod1$coefficients[grep("yday", names(mod1$coefficients))] <- 0
      x$Value <- x$Value - mgcv::predict.gam(mod1, newdata = x)

      # step 2: model seasonal trend component:  allow seasonal trend to evolve.
      seasonal_f <- sprintf("Value ~ te(yday, day, bs = c('cc', 'tp'), k = c(6, %i))", k_t)
      mod2 <- mgcv::gam(stats::formula(seasonal_f), data = x)

      # replace Values with modelled ones
      x$Value <- mgcv::predict.gam(mod1, newdata = x) +
        mgcv::predict.gam(mod2, newdata = x)
    }

    # remove working columns
    x %<>% dplyr::select(-.data$yday, -.data$day)
  }
  x
}

estimate_variable_values <- function(data, by = NULL, variables = c("Chloride Total", "Hardness Total", "pH"),
                                     messages = getOption("wqbc.messages", default = TRUE)) {
  check_data(data, values = list(Date = Sys.Date(), Variable = "", Value = c(1, NA), Units = ""))
  chkor(chk_null(by), check_values(by, ""))
  check_values(variables, "")
  if (!all(variables %in% c("Chloride Total", "Hardness Total", "pH"))) error("Unrecognized variables")
  chk_flag(messages)
  stopifnot(!any(c("Variable", "Value", "Units", "Date") %in% by))

  variables <- unique(variables)

  dates_by <- data[unique(c("Date", by))] %>% dplyr::distinct()

  for (variable in variables) {
    if (messages) message("Estimating ", variable)

    new_data <- dplyr::filter(data, .data$Variable == variable)

    if (nrow(new_data)) {
      new_data %<>% standardize_wqdata(messages = messages)

      unit <- new_data$Units[1]

      new_data %<>% dplyr::right_join(dates_by, by = c("Date", by))

      new_data$Units <- unit
      new_data$Variable <- variable

      if (is.null(by)) {
        new_data %<>% estimate_variable_values_by(messages = messages)
      } else {
        new_data %<>% plyr::ddply(
          .variables = by, .fun = estimate_variable_values_by,
          messages = messages
        )
      }

      data %<>% dplyr::filter(.data$Variable != variable)

      data %<>% dplyr::bind_rows(new_data)

      if (messages) message("Estimated ", variable, " values")
    } else if (messages) message("No ", variable, " values")
  }

  data
}
