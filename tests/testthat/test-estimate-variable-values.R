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

context("estimate-variable-values")

test_that("delete_outliers vary data", {
fraser <- wqbc::fraser
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
fit   <- estimate_variable_values(fraser,   by = "Station", messages = FALSE)

expect_identical(colnames(fit), colnames(fraser))
expect_identical(nrow(fit), nrow(fraser))

fit2  <- estimate_variable_values(fraser2,  by = "Station", message = FALSE)
fit2a <- estimate_variable_values(fraser2a, by = "Station", message = FALSE)
fit1  <- estimate_variable_values(fraser1,  by = "Station", message = FALSE)
fit1a <- estimate_variable_values(fraser1a, by = "Station", message = FALSE)
fit1b <- estimate_variable_values(fraser1b, by = "Station", message = FALSE)

# check predictions are near origional values
# max % abs diff
expect_lt(max(abs(fit$Value/fraser$Value - 1)), 0.32)
expect_lt(max(abs(fit2$Value/fraser2$Value - 1)), 0.25)
expect_lt(max(abs(fit2a$Value[!is.na(fraser2a$Value)]/fraser2a$Value[!is.na(fraser2a$Value)] - 1)), 0.25)
expect_true(all(!is.na(fit2a$Value)))
expect_lt(max(abs(fit1$Value/fraser1$Value - 1)), 0.25)
expect_lt(max(abs(fit1b$Value/fraser1$Value - 1)), 0.25)
# mean % abs diff
expect_lt(mean(abs(fit$Value/fraser$Value - 1)), 0.05)
expect_lt(mean(abs(fit2$Value/fraser2$Value - 1)), 0.05)
expect_lt(mean(abs(fit2a$Value/fraser2$Value - 1)), 0.05)
expect_lt(mean(abs(fit1$Value/fraser1$Value - 1)), 0.05)
expect_lt(mean(abs(fit1b$Value/fraser1$Value - 1)), 0.06)
# should give mean
expect_equal(fit1a$Value[1], mean(fraser1a$Value, na.rm = TRUE))

# test inputs
expect_error(estimate_variable_values(select_(fraser1, ~-Station)))
expect_error(estimate_variable_values(select_(fraser1, ~-Value)))
expect_error(estimate_variable_values(select_(fraser1, ~-Variable)))
expect_error(estimate_variable_values(select_(fraser1, ~-Unit)))
expect_error(estimate_variable_values(select_(fraser1, ~-Date)))

# check no predictions are made if no Hardness Total obs are present
expect_identical(estimate_variable_values(fraser1c, messages = FALSE), fraser1c)
})
