context("lookup")

test_that("lookup_units", {
  opts <- options()
  on.exit(options(opts))
  options(wqbc.messages = FALSE)

  expect_is(lookup_units(), "character")

})

test_that("lookup_codes lookup_variables", {
  opts <- options()
  on.exit(options(opts))
  options(wqbc.messages = FALSE)

  expect_identical(lookup_codes(lookup_variables(c(lookup_codes(),NA,"KRYP"))),
                   c(lookup_codes(), NA, NA))
  expect_identical(lookup_variables(lookup_codes(c(lookup_variables(),NA,"Kryptonite"))),
                   c(lookup_variables(), NA, NA))
})
