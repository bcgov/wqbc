# Copyright 2016 Province of British Columbia
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

context("identify-outliers")

test_that("outlier_sense_check", {

  x <- data.frame(Value = 1:10, Outlier = TRUE)

  expect_false(outlier_sense_check(x))
  x$Outlier <- FALSE
  expect_true(outlier_sense_check(x))
  expect_false(outlier_sense_check(x[1:2,]))
  expect_true(outlier_sense_check(x[1:3,]))
})

test_that("identify_outliers time_series = FALSE", {
  expect_error(identify_outliers(wqbc::dummy), "column Value in data cannot include missing values")
  data <- standardize_wqdata(wqbc::dummy, messages = FALSE)
  expect_message(identify_outliers(data, messages = TRUE), "Identified 3 outliers in water quality data.")
})
