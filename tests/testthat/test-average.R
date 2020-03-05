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

context("average")

test_that("geomean1", {
  expect_equal(geomean1(1), 1)
  expect_equal(geomean1(100), 100)
  expect_equal(geomean1(c(100, 100)), 100)
  expect_equal(geomean1(c(100, NA), na.rm = TRUE), 100)
  expect_true(is.na(geomean1(c(100, NA))))
  expect_true(is.na(geomean1(NA)))
  expect_true(is.nan(geomean1(NA, na.rm = TRUE)))
  expect_true(is.nan(geomean1(NA, na.rm = TRUE)))
  expect_error(geomean1(-1))
  expect_equal(geomean1(0:9), 3.528729, tolerance = 10^-6)
})
