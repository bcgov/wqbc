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
})
