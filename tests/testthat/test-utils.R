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
  expect_message(add_missing_columns(ccme, list(Test = NA_real_)))
  expect_equal(ccme, add_missing_columns(ccme, list(Date = as.Date("2000-01-01"))))
})

test_that("delete_rows_with_missing_values", {
  x <- data.frame(X = c(1,2,NA,4,NA), Y = c(1,NA,NA,4,5), Z = 1:5)

  expect_message(delete_rows_with_missing_values(x, list("X", "Y")))
  z <- delete_rows_with_missing_values(x, list("X", "Y"), messages = FALSE)
  expect_identical(x[!is.na(x$X) & !is.na(x$Y),,drop = FALSE], z)
  z <- delete_rows_with_missing_values(x, list(c("X", "Y")), messages = FALSE)
  expect_identical(x[!(is.na(x$X) & is.na(x$Y)),,drop = FALSE], z)
})

test_that("replace_negative_values_with_na", {
  expect_message(replace_negative_values_with_na(-1))
  expect_equal(replace_negative_values_with_na(c(1,NA,-1,2), messages = FALSE), c(1,NA,NA,2))
})
