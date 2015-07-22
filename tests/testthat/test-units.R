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

context("units")

test_that("get_unit_multiplier", {
  expect_equal(get_unit_multiplier(c("mg/L", "kg/L", "g/L", NA)),
               c(10^-3, 10^3, 1, NA))

})

test_that("get_unit_type", {
  opts <- options()
  on.exit(options(opts))
  options(wqbc.messages = FALSE)

  expect_equal(get_unit_type(c("mg/L", "ug/L", "pH")),
               c("concentration", "concentration", "pH"))
})

test_that("convert_values", {
  expect_equal(convert_values(1, "mg/L", "mg/L", messages = FALSE), 1)
  expect_equal(convert_values(10, "mg/L", "mg/L", messages = FALSE), 10)
  expect_equal(convert_values(1, "ug/L", "mg/L", messages = FALSE), 10^-3)
  expect_equal(convert_values(1, "ug/L", "kg/L", messages = FALSE), 10^-9)
  expect_equal(convert_values(1, "kg/L", "ug/L", messages = FALSE), 10^9)
  expect_equal(convert_values(1, "pH", "pH", messages = FALSE), 1)

  expect_equal(convert_values(c(0.1, 1, 10), "mg/L", "ug/L", messages = FALSE), c(10^2, 10^3, 10^4))
  expect_equal(convert_values(c(0.1, 1, 10), c("mg/L", "ug/L", "kg/L"), "ug/L", messages = FALSE), c(10^2, 1, 10^10))
  expect_equal(convert_values(c(0.1, 1, 10), "ug/L", c("mg/L", "ug/L", "kg/L"), messages = FALSE), c(10^-4, 1, 10^-8))
  expect_warning(convert_values(c(0.1, 1, 10), "ug/L", c("mg/L", "ug/L"), messages = FALSE))
  expect_warning(convert_values(c(0.1, 1, 10), "ug/L", c("mg/L", "ug/L"), messages = FALSE))
  expect_true(is.na(convert_values(NA_real_, "ug/L", "ug/L", messages = FALSE)))
  expect_equal(convert_values(c(1, NA), "ug/L", "mg/L", messages = FALSE), c(10^-3, NA))
#  expect_warning(convert_values(c(0.1, 1, 10), "ug/L", "xx"))
})
