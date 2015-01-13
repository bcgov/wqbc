context("substitute")

test_that("substitute_units", {
  expect_equal(substitute_units(c("mg/L", "MG/L", "mg /L ", "Kg/l"), messages = FALSE),
               c("mg/L", "mg/L", "mg/L", "kg/L"))
  expect_equal(substitute_units("mg.L"), "mg.L")
  expect_identical(substitute_units(c("MG /L", NA), messages = FALSE), c("mg/L", NA))
})

test_that("substitute_variables", {
  expect_equal(substitute_variables(c("ALUMINIUM SOMETHING", "FLUORIDE DISSOLVED",
                                      "FLUORIDE", "NITROGEN DISSOLVED NITRATE",
                                      "PHOSPHORUS - TOTAL",
                                     "KRYPTONITE", "OXYGEN", "OXYGEN DISSOLVED"),
               messages = FALSE), c("Aluminium", "Fluoride", "Fluoride", "Nitrate",
                                    "Total Phosphorus", "KRYPTONITE", "OXYGEN",
                                    "Dissolved Oxygen"))
  expect_equal(substitute_variables("KRYPTONITE", messages = FALSE), "KRYPTONITE")
  expect_equal(substitute_variables("NITROGEN DISSOLVED NITRATE", messages = FALSE), "Nitrate")
  expect_message(substitute_variables("NITROGEN DISSOLVED NITRATE"), "Nitrate")
})

