context("calc-limits")

test_that("calc_limits single limit", {

  date <- as.Date("2000-01-01")
  variable <- "Phosphorus Total"
  df <- data.frame(Date = date, Variable = variable, Value = 5)
  expect_error(calc_limits(df, messages = TRUE))
  df$Units <- "mg/L"
  x <- calc_limits(df)
  expect_is(x, "data.frame")
  expect_identical(colnames(x), c("Date", "Variable", "Value", "LowerLimit","UpperLimit", "Units"))
  expect_identical(as.character(x$Units), "ug/L")
  expect_identical(x$Value, 5000)
  expect_identical(x$LowerLimit, 5)
  expect_identical(x$UpperLimit, 10)
})
