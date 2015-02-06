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

test_that("substitute_variables strict", {
  opts <- options()
  on.exit(options(opts))
  options(wqbc.messages = FALSE)

  expect_equal(substitute_variables(c("ALUMINIUM SOMETHING", "ALUMINUM DISSOLVED",
                                      "dissolved aluminium", "BORON Total",
                                     "KRYPTONITE"),
               strict = TRUE), c(NA, "Aluminium Dissolved", "Aluminium Dissolved", "Boron", NA))
  expect_equal(substitute_variables("KRYPTONITE", strict = TRUE), NA_character_)
  expect_message(substitute_variables("DISSOLVED ALUMINIUM", strict = TRUE, messages = TRUE))
})

test_that("substitute_variables not strict", {
  opts <- options()
  on.exit(options(opts))
  options(wqbc.messages = FALSE)

#   expect_equal(substitute_variables(c("ALUMINIUM SOMETHING", "ALUMINUM DISSOLVED",
#                                       "dissolved aluminium", "BORON Total",
#                                      "KRYPTONITE"),
#                strict = FALSE), c(NA, "Aluminium Dissolved", "Aluminium Dissolved", "Boron", NA))
  expect_equal(substitute_variables("KRYPTONITE", strict = FALSE), NA_character_)
  expect_message(substitute_variables("DISSOLVED ALUMINIUM", strict = FALSE, messages = TRUE))
})

