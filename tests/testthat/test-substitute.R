context("substitute")

test_that("substitute_units", {
  opts <- options()
  on.exit(options(opts))
  options(wqbc.messages = FALSE)

  expect_equal(substitute_units(c("mg/L", "MG/L", "mg /L ", "Kg/l")),
               c("mg/L", "mg/L", "mg/L", "kg/L"))
  expect_equal(substitute_units("mg.L"), NA_character_)
  expect_identical(substitute_units(c("MG /L", NA)), c("mg/L", NA))
})

test_that("substitute_variables", {
  opts <- options()
  on.exit(options(opts))
  options(wqbc.messages = FALSE)

  expect_equal(substitute_variables(c("ALUMINIUM SOMETHING", "FLUORIDE DISSOLVED",
                                      "TOTAL FLUORIDE", "NITROGEN DISSOLVED NITRATE",
                                     "KRYPTONITE"),
               strict = TRUE), c("ALUMINIUM SOMETHING", "FLUORIDE DISSOLVED", "Fluoride Total", "NITROGEN DISSOLVED NITRATE","KRYPTONITE"))
  expect_equal(substitute_variables("KRYPTONITE", strict = TRUE), "KRYPTONITE")
  expect_equal(substitute_variables("DISSOLVED ALUMINIUM", strict = TRUE), "Aluminium Dissolved")
  expect_message(substitute_variables("DISSOLVED ALUMINIUM", strict = TRUE, messages = TRUE))
})
