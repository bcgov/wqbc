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

context("calc-limits")

test_that("calc_limits single limit", {
  opts <- options()
  on.exit(options(opts))
  options(wqbc.messages = FALSE)

  date <- as.Date("2000-01-01")
  variable <- "Cobalt Total"
    df <- data.frame(Date = date, Variable = variable, Value = 5)
    expect_error(calc_limits(df))
    df$Units <- "mg/L"
    x <- calc_limits(df)
    expect_is(x, "data.frame")
    expect_identical(colnames(x), c("Date", "Variable", "Value", "UpperLimit", "Units"))
    expect_equal(nrow(x), 0)
    x <- calc_limits(df, term = "short")
    expect_is(x, "data.frame")
    expect_identical(colnames(x), c("Date", "Variable", "Value", "UpperLimit", "Units"))
    expect_identical(as.character(x$Units), "ug/L")
    expect_identical(x$Value, 5000)
    expect_identical(x$UpperLimit, 110)
    df <- rbind(df,df)
    df$UpperLimit <- c(NA, 4)
    x <- calc_limits(df, term = "short")
    expect_identical(colnames(x), c("Date", "Variable", "Value", "UpperLimit", "Units"))
    expect_identical(as.character(x$Units), c("ug/L", "mg/L"))
    expect_identical(x$Value, c(5000,5))
    expect_identical(x$UpperLimit, c(110,4))
})

test_that("calc_limits dependent", {
  opts <- options()
  on.exit(options(opts))
  options(wqbc.messages = FALSE)

  date <- as.Date("2000-01-01")
  variable <- "Copper Total"
  df <- data.frame(Date = date, Variable = variable, Value = 5, Units = "mg/L")
  x <- calc_limits(df, term = "long-daily")
  expect_is(x, "data.frame")
  expect_identical(colnames(x), c("Date", "Variable", "Value", "UpperLimit", "Units"))
  expect_equal(nrow(x), 0)

  df2 <- data.frame(Date = date, Variable = "Hardness Total", Value = 100, Units = "mg/L")
  df3 <- rbind(df, df2)
  x <- calc_limits(df3, term = "long-daily")
  expect_equal(nrow(x), 1L)
  expect_equal(x$UpperLimit, 4)

  df2 <- data.frame(Date = date + 1, Variable = "Hardness Total", Value = 100, Units = "mg/L")
  df3 <- rbind(df, df2)
  x <- calc_limits(df3, term = "long-daily")
  expect_equal(nrow(x), 1L)
  expect_equal(x$UpperLimit, 4)

  df2 <- data.frame(Date = date, Variable = "Hardness Total", Value = 200, Units = "mg/L")
  df3 <- rbind(df3, df2)
  x <- calc_limits(df3, term = "long-daily")
  expect_equal(nrow(x), 1L)
  expect_equal(x$UpperLimit, 8)

  df3$Date[3] <- df3$Date[3] + 1
  x <- calc_limits(df3, term = "long-daily")
  expect_equal(nrow(x), 1L)
  expect_equal(x$UpperLimit, 6)
})

test_that("calc_limits copper dependent", {
  opts <- options()
  on.exit(options(opts))
  options(wqbc.messages = FALSE)

  copper <- data.frame(Variable = "Copper Total", Value = 0.9, Units = "ug/L", Date = as.Date("2000-01-01"))
  hardness <- data.frame(Variable = "Hardness Total", Value = 69.2, Units = "mg/L", Date = as.Date("2000-01-01"))

  df <- rbind(copper, hardness)

  x <- calc_limits(df, term = "long-daily", estimate_variables = TRUE)

#  x <- calc_limits(df, term = "long-daily")
  expect_is(x, "data.frame")
  expect_identical(colnames(x), c("Date", "Variable", "Value", "UpperLimit", "Units"))
  expect_equal(nrow(x), 1)
  expect_equal(x$UpperLimit, 2.768)
})
