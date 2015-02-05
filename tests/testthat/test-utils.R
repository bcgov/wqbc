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

test_that("replace_negative_values_with_na", {
  expect_message(replace_negative_values_with_na(-1, messages = TRUE))
  expect_equal(replace_negative_values_with_na(c(1,NA,-1,2), messages = FALSE), c(1,NA,NA,2))
})

test_that("geomean1", {
  expect_equal(geomean1(1), 1)
  expect_equal(geomean1(100), 100)
  expect_equal(geomean1(c(100, 100)), 100)
  expect_equal(geomean1(c(100, NA), na.rm = TRUE), 100)
  expect_true(is.na(geomean1(c(100, NA))))
  expect_true(is.na(geomean1(NA)))
  expect_true(is.nan(geomean1(NA, na.rm =TRUE)))
  expect_true(is.nan(geomean1(NA, na.rm =TRUE)))
  expect_error(geomean1(-1))
  expect_equal(geomean1(0:9), 3.528729, tolerance = 10^-6)
})
