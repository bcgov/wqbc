# Copyright 2015 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

context("substitute")

test_that("substitute_units", {
  opts <- options()
  on.exit(options(opts))
  options(wqbc.messages = FALSE)

  expect_equal(substitute_units(c("mg/L", "MG/L", "mg /L ", "Kg/l")),
               c("mg/L", "mg/L", "mg/L", "kg/L"))
  expect_equal(substitute_units(c("DEG C")), c("degC"))
  expect_equal(substitute_units(c("Col.unit")), c("Col.Unit"))
  expect_equal(substitute_units("mg.L"), NA_character_)
  expect_identical(substitute_units(c("MG /L", NA)), c("mg/L", NA))
})

test_that("substitute_variables strict", {
  opts <- options()
  on.exit(options(opts))
  options(wqbc.messages = FALSE)

  expect_equal(substitute_variables(c("ALUMINIUM SOMETHING", "ALUMINUM DISSOLVED",
                                      "dissolved aluminium", "Benzene Total",
                                     "KRYPTONITE"),
               strict = TRUE), c(NA, "Aluminium Dissolved", "Aluminium Dissolved", "Benzene", NA))
  expect_equal(substitute_variables("KRYPTONITE", strict = TRUE), NA_character_)
  expect_equal(substitute_variables(c("Benzene Total", "Alkalinity pH 4.5/4.2",
                                      "Dissolved Aluminium", "Hardness Dissolved Total", "pH")),
               c("Benzene", "Alkalinity pH 4.5/4.2", "Aluminium Dissolved",
                 NA, "pH"))
  expect_message(substitute_variables("DISSOLVED ALUMINIUM", strict = TRUE, messages = TRUE))
})

test_that("substitute_variables not strict", {
  opts <- options()
  on.exit(options(opts))
  options(wqbc.messages = FALSE)

   expect_equal(substitute_variables(c("ALUMINIUM SOMETHING", "ALUMINUM DISSOLVED",
                                       "dissolved aluminium", "Benzene Total",
                                      "KRYPTONITE"),
                strict = FALSE), c("Aluminium Dissolved", "Aluminium Dissolved", "Aluminium Dissolved", "Benzene", NA))
  expect_equal(substitute_variables("KRYPTONITE", strict = FALSE), NA_character_)
  expect_equal(substitute_variables(c("Iron Total", "Iron Dissolved", "Aluminum"), strict = TRUE),
               c("Iron Total", "Iron Dissolved", NA))
  expect_equal(substitute_variables(c("Iron Total", "Iron Dissolved", "Aluminum"), strict = FALSE),
               c(NA, NA, "Aluminium Dissolved"))
  expect_equal(substitute_variables("KRYPTONITE", strict = FALSE), NA_character_)

})

