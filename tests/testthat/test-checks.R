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

context("checks")

test_that("check_class_columns", {
  data(ccme)
  expect_true(check_class_columns(ccme, list(Date = "Date")))
  expect_error(check_class_columns(ccme, list(Date = "factor")))
  expect_true(check_class_columns(ccme, list(Variable = c("character", "factor"))))
})

test_that("check_by", {
  expect_true(check_by(NULL, "x", "y"))
  expect_true(check_by("x", "x", "y"))
  expect_error(check_by("y", "x", "y"))
  expect_error(check_by("z", "x", "y"))
})
