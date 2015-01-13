context("checks")

test_that("check_rows", {
  expect_error(check_rows(data.frame()))
  data(ccme)
  expect_true(check_rows(ccme))
})

test_that("check_columns", {
  data(ccme)
  expect_error(check_columns(ccme, c("Random")))
  expect_true(check_columns(ccme, c("Variable", "Date", "UpperLimit")))
})

test_that("check_class_columns", {
  data(ccme)
  expect_true(check_class_columns(ccme, list(Date = "Date")))
  expect_error(check_class_columns(ccme, list(Date = "factor")))
  expect_true(check_class_columns(ccme, list(Variable = c("character", "factor"))))
})
check_class_columns
