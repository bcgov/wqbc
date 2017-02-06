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

  # filter out total hardness observations
  x_ht <- x %>% dplyr::filter_(~Variable == "Hardness Total")

  # identify all sampled dates in data and join onto x_ht data
  # it is nessisary to include Station as this cannot be guessed
  x_ht <- x %>% dplyr::select_(~Date,~Station) %>%
                unique(.) %>%
                dplyr::left_join(x_ht, by = c("Date", "Station"))
  x_ht$Units[is.na(x_ht$Units)] <- x_ht$Units[1]
  x_ht$Variable[is.na(x_ht$Variable)] <- "Hardness Total"

  # if there is not enough observations, then skip modelling and return with NAs
  # here defined as 6 x the year range
  if (sum(!is.na(x_ht$Value)) > (diff(range(lubridate::year(x_ht$Date))) + 1) * 6 ) {
    # extract seasonal covariates from 'Date'
    x_ht %<>% dplyr::mutate_(yday = ~lubridate::yday(Date), # for seasonal trend
                              day = ~lubridate::decimal_date(Date)) # for long term trends

    # step 1: long term trend, set k to number of years
    #         (could possibly go up to 1.5 * no. of years)
    # note - also fit a seasonal model to allow for unbalanced sampling
    #        within some years.
    k_t <- diff(range(lubridate::year(x_ht$Date))) + 1L
    trend_f <- sprintf("Value ~ s(day, k = %i) + s(yday, bs = 'cc', k = 6)", as.integer(k_t))
    mod1 <- mgcv::gam(stats::formula(trend_f), data = x_ht)
    ## set yday coeffs to zero to remove them from the model
    mod1$coefficients[grep("yday", names(mod1$coefficients))] <- 0
    x_ht$Value_trend <- mgcv::predict.gam(mod1, newdata = x_ht)

    # step 2: model seasonal trend component:  allow seasonal trend to evolve.
    seasonal_f <- sprintf("I(Value - Value_trend) ~ te(yday, day, bs = c('cc', 'tp'), k = c(6, %i))", k_t)
    mod2 <- mgcv::gam(stats::formula(seasonal_f), data = x_ht)
    x_ht$Value_season <- mgcv::predict.gam(mod2, newdata = x_ht)

    # replace Values with modelled ones
    x_ht %<>% dplyr::mutate_(Value = ~Value_trend + Value_season)

    # remove working columns
    x_ht %<>% dplyr::select_(~-Value_season, ~-Value_trend, ~-yday, ~-day)
  } else {
    # not enough non-NA observations
  }
  # join back onto full data and return
  x %>% dplyr::filter_(~Variable != "Hardness Total") %>%
        dplyr::full_join(x_ht, by = names(x))
}

model_totalhardness <- function(data, by = NULL,
                                messages = getOption("wqbc.messages", default = TRUE)) {

  # check the contents of data, all we require is Value, Date
  # ... and by variables ...
  check_data2(data, values = list(Value = c(1, NA),
                                  Date = as.Date("2014/1/1"),
                                  Variable = ""))
  if (is.null(by)) {
    data %<>% model_totalhardness_by(messages = messages)
  } else {
    data %<>% plyr::ddply(.variables = by, .fun = model_totalhardness_by,
                          messages = messages)
  }

  if (messages) message("Replaced water hardness observations with modelled values.")

  data
}
