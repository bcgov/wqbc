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


model_totalhardness_by <- function(x, messages) {

  # extract seasonal covariates form 'Date'
  x %<>% dplyr::mutate_(yday = ~lubridate::yday(Date), # for seasonal trend
                         day = ~lubridate::decimal_date(Date)) # for long term trends

  # step 1: long term trend, set k to number of years
  #         (could possibly go up to 1.5 * no. of years)
  # note - also fit a seasonal model to allow for unbalanced sampling
  #        within some years.
  k_t <- diff(range(lubridate::year(x$Date))) + 1L
  trend_f <- sprintf("Value ~ s(day, k = %i) + s(yday, bs = 'cc', k = 6)", as.integer(k_t))
  mod1 <- mgcv::gam(stats::formula(trend_f), data = x)
  ## set yday coeffs to zero to remove them from the model
  mod1$coefficients[grep("yday", names(mod1$coefficients))] <- 0
  x$Value_trend <- mgcv::predict.gam(mod1)

  # step 2: model seasonal trend component:  allow seasonal trend to evolve.
  seasonal_f <- sprintf("I(Value - Value_trend) ~ te(yday, day, bs = c('cc', 'tp'), k = c(6, %i))", k_t)
  mod2 <- mgcv::gam(stats::formula(seasonal_f),
                    data = x)
  x$Value_season <- mgcv::predict.gam(mod2)

  # save predictions
  x %<>% dplyr::mutate_(Value_modelled = ~Value_trend + Value_season)

  # record how unusual some observations are, so user can replace outliers if desired
  x %<>% dplyr::mutate_(Value_resid = ~(Value - Value_modelled) / sd(Value - Value_modelled))

  x %>% dplyr::select_(~-Value_season, ~-Value_trend, ~-yday, ~-day)
}

model_totalhardness <- function(data, by = NULL,
                                threshold = 3.5,
                                messages = getOption("wqbc.messages", default = TRUE)) {

  # check the contents of data, all we require is Value, Date
  # ... and by variables ...
  check_data2(data, values = list(Value = c(1, NA),
                                  Date = as.Date("2014/1/1")))

  if (is.null(by)) {
    data %<>% model_totalhardness_by(messages = messages)
  } else {
    data %<>% plyr::ddply(.variables = by, .fun = model_totalhardness_by,
                          messages = messages)
  }

  # fillin appropriate Values with modelled ones
  bool <- is.na(data$Value) | abs(data$Value_resid) > threshold
  data$Value[bool] <- data$Value_modelled[bool]

  #  inform user of the number of values imputed / replaced
  if (messages) message("Replaced ", sum(bool),
                        " NA or unusual water hardness observations with modelled ones.")

  data # %>% dplyr::select_(~-Value_resid)
}
