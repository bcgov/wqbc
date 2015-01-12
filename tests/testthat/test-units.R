context("units")

test_that("get_unit_multiplier", {
  expect_equal(get_unit_multiplier(c("mg/L", "kg/L", "g/L", NA, "m")),
               c(10^-3, 10^3, 1, NA, 1))

})

test_that("get_units", {
  expect_is(get_units(), "character")
})

test_that("get_unit_type", {
  expect_equal(get_unit_type(c("mg/L", "ug/L", "/100mL", "m", "pH", "NTU")),
               c("concentration", "concentration", "individuals", "length", "pH",
                 "turbidity"))
})

test_that("convert_units", {
  expect_equal(convert_units(1, "mg/L", "mg/L"), 1)
  expect_equal(convert_units(10, "mg/L", "mg/L"), 10)
  expect_equal(convert_units(1, "ug/L", "mg/L"), 10^-3)
  expect_equal(convert_units(1, "ug/L", "kg/L"), 10^-9)
  expect_equal(convert_units(1, "kg/L", "ug/L"), 10^9)
  expect_equal(convert_units(1, "mm", "m"), 10^-3)
  expect_equal(convert_units(1, "pH", "pH"), 1)

  expect_equal(convert_units(c(0.1, 1, 10), "mg/L", "ug/L"), c(10^2, 10^3, 10^4))
  expect_equal(convert_units(c(0.1, 1, 10), c("mg/L", "ug/L", "kg/L"), "ug/L"), c(10^2, 1, 10^10))
  expect_equal(convert_units(c(0.1, 1, 10), "ug/L", c("mg/L", "ug/L", "kg/L")), c(10^-4, 1, 10^-8))
  expect_warning(convert_units(c(0.1, 1, 10), "ug/L", c("mg/L", "ug/L")))
  expect_warning(convert_units(c(0.1, 1, 10), "ug/L", c("mg/L", "ug/L")))
  expect_true(is.na(convert_units(NA_real_, "ug/L", "ug/L")))
  expect_equal(convert_units(c(1, NA), "ug/L", "mg/L"), c(10^-3, NA))
#  expect_warning(convert_units(c(0.1, 1, 10), "ug/L", "xx"))
})
