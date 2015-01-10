context("utils")

test_that("punctuate_strings", {
  expect_identical(punctuate_strings(c("x")), "x")
  expect_identical(punctuate_strings(c("x","y")), "x or y")
  expect_identical(punctuate_strings(c("x","y","z")), "x, y or z")
  expect_identical(punctuate_strings(c("x","y","z","a")), "x, y, z or a")
  expect_identical(punctuate_strings(c("x","y","z","a"), "and"), "x, y, z and a")
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
