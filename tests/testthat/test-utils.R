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

context("utils")

test_that("punctuate_strings", {
  expect_identical(punctuate_strings(c("x")), "x")
  expect_identical(punctuate_strings(c("x","y")), "x or y")
  expect_identical(punctuate_strings(c("x","y","z")), "x, y or z")
  expect_identical(punctuate_strings(c("x","y","z","a")), "x, y, z or a")
  expect_identical(punctuate_strings(c("x","y","z","a"), "and"), "x, y, z and a")
})

test_that("add_missing_columns", {
  data(ccme)

  expect_error(add_missing_columns(1))
  x <- add_missing_columns(ccme, list(Test = NA_real_), messages = FALSE)
  expect_is(x, "data.frame")
  expect_equal(colnames(x), c(colnames(ccme), "Test"))
  expect_message(add_missing_columns(ccme, list(Test = NA_real_), messages = TRUE))
  expect_equal(ccme, add_missing_columns(ccme, list(Date = as.Date("2000-01-01")), messages = FALSE))
})

test_that("delete_rows_with_certain_values", {
  x <- data.frame(X = c(1,2,NA,4,NA), Y = c(1,NA,NA,4,5), Z = 1:5)

  expect_message(delete_rows_with_certain_values(x, list("X", "Y"), messages = TRUE))
  z <- delete_rows_with_certain_values(x, list("X", "Y"), messages = FALSE)
  expect_identical(x[!is.na(x$X) & !is.na(x$Y),,drop = FALSE], z)
  z <- delete_rows_with_certain_values(x, list(c("X", "Y")), messages = FALSE)
  expect_identical(x[!(is.na(x$X) & is.na(x$Y)),,drop = FALSE], z)
})

test_that("is_color", {
  expect_true(is_color("black"))
  expect_false(is_color("Date"))
})

