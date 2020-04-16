test_that("errors correctly", {
  chk::expect_chk_error(summarise_wqdata(data.frame(Value = 1)))
  chk::expect_chk_error(summarise_wqdata(data.frame(Variable = 1, Value = 1)))
})

test_that("zero rows", {
  x <- summarise_wqdata(data.frame(Variable = character(0), Value = double(0), stringsAsFactors = FALSE))
  y <-  tibble::tibble(
    Variable = character(0),
    n = integer(0),
    min = double(0),
    max = double(0),
    mean = double(0),
    median = double(0),
    lowerQ = double(0),
    upperQ = double(0),
    sd = double(0),
    se = double(0))

  expect_identical(x,y)
})

test_that("works", {
  x <- summarise_wqdata(data.frame(Variable = "1", Value = 1, stringsAsFactors = FALSE))
  y <- tibble::tibble(Variable = "1", n = 1L, min = 1, max = 1, mean = 1,
                      median = 1, lowerQ = 1, upperQ = 1, sd = NA_real_, se = NA_real_)
  expect_identical(x, y)
})

test_that("all missing values", {
  x <- summarise_wqdata(data.frame(Variable = "1", Value = NA_real_, stringsAsFactors = FALSE))
  y <- tibble::tibble(Variable = "1", n = 1L, min = NA_real_, max = NA_real_, mean = NA_real_,
                      median = NA_real_, lowerQ = NA_real_, upperQ = NA_real_, sd = NA_real_, se = NA_real_)
  expect_identical(x, y)
})

test_that("some missing values", {
  x <- summarise_wqdata(data.frame(Variable = "1", Value = c(NA_real_, 1), stringsAsFactors = FALSE))
  y <- tibble::tibble(Variable = "1", n = 2L, min = NA_real_, max = NA_real_, mean = NA_real_,
                      median = NA_real_, lowerQ = NA_real_, upperQ = NA_real_, sd = NA_real_, se = NA_real_)
  expect_identical(x, y)
})

test_that("na.rm = TRUE with some missing values", {
  x <- summarise_wqdata(data.frame(Variable = "1", Value = c(NA_real_, 1), stringsAsFactors = FALSE),
                        na.rm = TRUE)
  y <- tibble::tibble(Variable = "1", n = 1L, min = 1, max = 1, mean = 1,
                      median = 1, lowerQ = 1, upperQ = 1, sd = NA_real_, se = NA_real_)
  expect_identical(x, y)
})

test_that("multiple variables", {
  x <- summarise_wqdata(data.frame(Variable = c("1", "three"), Value = c(1, 3), stringsAsFactors = FALSE))
  y <- tibble::tibble(Variable = c("1", "three"), n = c(1L, 1L), min = c(1, 3), max = c(1, 3), mean = c(1, 3),
                      median = c(1, 3), lowerQ = c(1, 3), upperQ = c(1, 3), sd = c(NA_real_, NA_real_), se = c(NA_real_, NA_real_))

  expect_identical(x, y)
})

test_that("multiple integer values", {
  x <- summarise_wqdata(data.frame(Variable = "var", Value = 1:5, stringsAsFactors = FALSE))
  y <- tibble::tibble(Variable = "var", n = 5L, min = 1L, max = 5L, mean = 3,
                      median = 3L, lowerQ = 1.1, upperQ = 4.9, sd = 1.58113883008419, se = 0.707106781186548)

  expect_equal(as.data.frame(x), as.data.frame(y))
})

test_that("missing values in variables", {
  x <- summarise_wqdata(data.frame(Variable = NA_character_, Value = 1, stringsAsFactors = FALSE))
  y <- tibble::tibble(Variable = NA_character_, n = 1L, min = 1, max = 1, mean = 1,
                      median = 1, lowerQ = 1, upperQ = 1, sd = NA_real_, se = NA_real_)

  expect_identical(x, y)
})

test_that("factor variable", {
  x <- summarise_wqdata(data.frame(Variable = factor("1"), Value = 1, stringsAsFactors = FALSE))
  y <- tibble::tibble(Variable = factor("1"), n = 1L, min = 1, max = 1, mean = 1,
                      median = 1, lowerQ = 1, upperQ = 1, sd = NA_real_, se = NA_real_)
  expect_identical(x, y)
})
