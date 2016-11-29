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

test_that("adequate_unique_outliers", {

  x <- data.frame(Value = 1:10, Outlier = TRUE)

  expect_false(adequate_unique_outliers(x))
  x$Outlier <- FALSE
  expect_true(adequate_unique_outliers(x))
  expect_false(adequate_unique_outliers(x[1:2,]))
  expect_true(adequate_unique_outliers(x[1:3,]))
  expect_false(adequate_unique_outliers(x[c(1:2,1:2),]))
})

test_that("identify_outliers", {
  data <- data.frame(Value2 = c(1,NA))
  expect_error(identify_outliers(data), "data must have column 'Value'")
  data$Value <- data$Value2
  expect_identical(identify_outliers(data, messages = FALSE)$Outlier, c(FALSE, NA))
  expect_message(identify_outliers(data, messages = TRUE), "Identified 0 outliers in water quality data.")
  expect_message(identify_outliers(wqbc::dummy, messages = TRUE), "Identified 3 outliers in water quality data.")
  expect_identical(identify_outliers(data.frame(Value = c(NA_real_, NA_real_)), messages = FALSE)$Outlier, c(NA, NA))
  data <- data.frame(Value = c(rep(100, 2), rep(0, 1000)))
  expect_message(identify_outliers(data, messages = TRUE), "Identified 0 outliers in water quality data.")
  expect_message(identify_outliers(data, ignore_zeros = FALSE, messages = TRUE), "Identified 0 outliers in water quality data.")
  data <- data.frame(Value = c(101, rep(100, 2), rep(0, 1000)))
  expect_message(identify_outliers(data, ignore_zeros = FALSE, messages = TRUE), "Identified 3 outliers in water quality data.")
  data <- data.frame(Value = c(-101, rep(100, 2), rep(0, 1000)))
  expect_message(identify_outliers(data, ignore_zeros = FALSE, messages = TRUE), "Identified 2 outliers in water quality data.")
  expect_message(identify_outliers(data, ignore_zeros = FALSE, large_only = FALSE, messages = TRUE), "Identified 3 outliers in water quality data.")

  data <- expand.grid(Value = c(-101, rep(100, 2), rep(0, 1000)), Other = c("x", "y"))
  expect_message(identify_outliers(data, by = "Other", ignore_zeros = FALSE, large_only = FALSE, messages = TRUE), "Identified 6 outliers in water quality data.")
})
