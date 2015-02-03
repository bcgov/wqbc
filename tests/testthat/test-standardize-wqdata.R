context("standardize-wqdata")

test_that("get_unit_multiplier", {
  expect_equal(get_unit_multiplier(c("mg/L", "kg/L", "g/L", NA)),
               c(10^-3, 10^3, 1, NA))

})

test_that("get_units", {
  opts <- options()
  on.exit(options(opts))
  options(wqbc.messages = FALSE)

  expect_is(get_units(), "character")

})

test_that("get_unit_type", {
  opts <- options()
  on.exit(options(opts))
  options(wqbc.messages = FALSE)

  expect_equal(get_unit_type(c("mg/L", "ug/L", "pH")),
               c("concentration", "concentration", "pH"))
})

test_that("convert_values", {
  expect_equal(convert_values(1, "mg/L", "mg/L", messages = FALSE), 1)
  expect_equal(convert_values(10, "mg/L", "mg/L", messages = FALSE), 10)
  expect_equal(convert_values(1, "ug/L", "mg/L", messages = FALSE), 10^-3)
  expect_equal(convert_values(1, "ug/L", "kg/L", messages = FALSE), 10^-9)
  expect_equal(convert_values(1, "kg/L", "ug/L", messages = FALSE), 10^9)
  expect_equal(convert_values(1, "pH", "pH", messages = FALSE), 1)

  expect_equal(convert_values(c(0.1, 1, 10), "mg/L", "ug/L", messages = FALSE), c(10^2, 10^3, 10^4))
  expect_equal(convert_values(c(0.1, 1, 10), c("mg/L", "ug/L", "kg/L"), "ug/L", messages = FALSE), c(10^2, 1, 10^10))
  expect_equal(convert_values(c(0.1, 1, 10), "ug/L", c("mg/L", "ug/L", "kg/L"), messages = FALSE), c(10^-4, 1, 10^-8))
  expect_warning(convert_values(c(0.1, 1, 10), "ug/L", c("mg/L", "ug/L"), messages = FALSE))
  expect_warning(convert_values(c(0.1, 1, 10), "ug/L", c("mg/L", "ug/L"), messages = FALSE))
  expect_true(is.na(convert_values(NA_real_, "ug/L", "ug/L", messages = FALSE)))
  expect_equal(convert_values(c(1, NA), "ug/L", "mg/L", messages = FALSE), c(10^-3, NA))
#  expect_warning(convert_values(c(0.1, 1, 10), "ug/L", "xx"))
})

test_that("get_variables_codes", {
  opts <- options()
  on.exit(options(opts))
  options(wqbc.messages = FALSE)

  expect_identical(get_codes(get_variables(c(get_codes(),NA,"KRYP"))),
                   c(get_codes(), NA, NA))
  expect_identical(get_variables(get_codes(c(get_variables(),NA,"Kryptonite"))),
                   c(get_variables(), NA, NA))
})

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

