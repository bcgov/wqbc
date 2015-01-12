context("substitute")

test_that("substitute_units", {
  expect_equal(substitute_units(c("mg/L", "MG/L", "mg /L ", "Kg/l"), messages = FALSE),
               c("mg/L", "mg/L", "mg/L", "kg/L"))
  expect_equal(substitute_units("mg.L"), "mg.L")
  expect_identical(substitute_units(c("MG /L", NA), messages = FALSE), c("mg/L", NA))
})
