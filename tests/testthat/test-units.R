context("units")

test_that("get_unit_multiplier", {
  expect_equal(get_unit_multiplier(c("mg/L", "kg/L", "g/L", NA)),
               c(10^-3, 10^3, 1, NA))

})

test_that("get_units", {
  expect_is(get_units(), "character")

})

test_that("get_unit_type", {
  expect_equal(get_unit_type(c("mg/L", "ug/L", "pH")),
               c("concentration", "concentration", "pH"))
})

test_that("convert_values", {
  expect_equal(convert_values(1, "mg/L", "mg/L"), 1)
  expect_equal(convert_values(10, "mg/L", "mg/L"), 10)
  expect_equal(convert_values(1, "ug/L", "mg/L"), 10^-3)
  expect_equal(convert_values(1, "ug/L", "kg/L"), 10^-9)
  expect_equal(convert_values(1, "kg/L", "ug/L"), 10^9)
  expect_equal(convert_values(1, "pH", "pH"), 1)

  expect_equal(convert_values(c(0.1, 1, 10), "mg/L", "ug/L"), c(10^2, 10^3, 10^4))
  expect_equal(convert_values(c(0.1, 1, 10), c("mg/L", "ug/L", "kg/L"), "ug/L"), c(10^2, 1, 10^10))
  expect_equal(convert_values(c(0.1, 1, 10), "ug/L", c("mg/L", "ug/L", "kg/L")), c(10^-4, 1, 10^-8))
  expect_warning(convert_values(c(0.1, 1, 10), "ug/L", c("mg/L", "ug/L")))
  expect_warning(convert_values(c(0.1, 1, 10), "ug/L", c("mg/L", "ug/L")))
  expect_true(is.na(convert_values(NA_real_, "ug/L", "ug/L")))
  expect_equal(convert_values(c(1, NA), "ug/L", "mg/L"), c(10^-3, NA))
#  expect_warning(convert_values(c(0.1, 1, 10), "ug/L", "xx"))
})
