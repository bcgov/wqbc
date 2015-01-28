context("substitute")

test_that("get_variables_codes", {
  expect_identical(get_codes(get_variables(c(get_codes(),NA,"KR"))),
                   c(get_codes(), NA, NA))
  expect_identical(get_variables(get_codes(c(get_variables(),NA,"Kryptonite"))),
                   c(get_variables(), NA, NA))
})

test_that("substitute_units", {
  expect_equal(substitute_units(c("mg/L", "MG/L", "mg /L ", "Kg/l"), messages = FALSE),
               c("mg/L", "mg/L", "mg/L", "kg/L"))
  expect_equal(substitute_units("mg.L"), "mg.L")
  expect_identical(substitute_units(c("MG /L", NA), messages = FALSE), c("mg/L", NA))
})

test_that("substitute_variables", {
  expect_equal(substitute_variables(c("ALUMINIUM SOMETHING", "FLUORIDE DISSOLVED",
                                      "TOTAL FLUORIDE", "NITROGEN DISSOLVED NITRATE",
                                     "KRYPTONITE"),
               messages = FALSE), c("ALUMINIUM SOMETHING", "FLUORIDE DISSOLVED", "Fluoride Total", "NITROGEN DISSOLVED NITRATE","KRYPTONITE"))
  expect_equal(substitute_variables("KRYPTONITE", messages = FALSE), "KRYPTONITE")
  expect_equal(substitute_variables("DISSOLVED ALUMINIUM", messages = FALSE), "Aluminium Dissolved")
  expect_message(substitute_variables("DISSOLVED ALUMINIUM"))
})

