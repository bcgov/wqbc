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

context("model-totalhardness")

test_that("model-totalhardness vary data", {
data(fraser)
fraser$Station <-  factor(sub("BC08", "", as.character(fraser$SiteID)))
fraser <- dplyr::filter_(fraser, ~grepl("hardness", tolower(Variable)))
fraser <- dplyr::filter_(fraser, ~Station %in% "LF0001")
fraser <- clean_wqdata(fraser, by = "Station", message = FALSE)

# create degraded data
fraser2 <- dplyr::filter_(fraser, ~lubridate::year(Date) %in% 2012:2013)
fraser1 <- dplyr::filter_(fraser, ~lubridate::year(Date) %in% 2012)
fraser2a <- dplyr::mutate_(fraser2, Value = ~ifelse(lubridate::year(Date) == 2013 & lubridate::month(Date) > 5, NA, Value))
fraser1a <- dplyr::mutate_(fraser1, Value = ~ifelse(lubridate::month(Date) > 5, NA, Value))
fraser1b <- dplyr::mutate_(fraser1, Value = ~ifelse(1:nrow(fraser1) %% 2 == 0, NA, Value))
fraser1c <- dplyr::mutate(fraser1, Variable = "Nothing")

# fit models
fit   <- model_totalhardness(fraser,   by = "Station", message = FALSE)$Value
fit2  <- model_totalhardness(fraser2,  by = "Station", message = FALSE)$Value
fit2a <- model_totalhardness(fraser2a, by = "Station", message = FALSE)$Value
fit1  <- model_totalhardness(fraser1,  by = "Station", message = FALSE)$Value
fit1a <- model_totalhardness(fraser1a, by = "Station", message = FALSE)$Value
fit1b <- model_totalhardness(fraser1b, by = "Station", message = FALSE)$Value

# check predictions are near origional values
# max % abs diff
expect_lt(max(abs(fit/fraser$Value - 1)), 0.32)
expect_lt(max(abs(fit2/fraser2$Value - 1)), 0.25)
expect_lt(max(abs(fit2a/fraser2$Value - 1)), 0.25)
expect_lt(max(abs(fit1/fraser1$Value - 1)), 0.25)
expect_lt(max(abs(fit1b/fraser1$Value - 1)), 0.25)
# mean % abs diff
expect_lt(mean(abs(fit/fraser$Value - 1)), 0.05)
expect_lt(mean(abs(fit2/fraser2$Value - 1)), 0.05)
expect_lt(mean(abs(fit2a/fraser2$Value - 1)), 0.05)
expect_lt(mean(abs(fit1/fraser1$Value - 1)), 0.05)
expect_lt(mean(abs(fit1b/fraser1$Value - 1)), 0.06)
# should give mean
expect_equal(fit1a[1], mean(fraser1a$Value, na.rm = TRUE))

# test inputs
expect_error(model_totalhardness(select_(fraser1, ~-Station)))
expect_error(model_totalhardness(select_(fraser1, ~-Value)))
expect_error(model_totalhardness(select_(fraser1, ~-Variable)))
expect_error(model_totalhardness(select_(fraser1, ~-Unit)))
expect_error(model_totalhardness(select_(fraser1, ~-Date)))

# check no predictions are made if no Hardness Total obs are present
expect_identical(model_totalhardness(fraser1c, messages = FALSE), fraser1c)
})
