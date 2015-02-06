context("standardize-wqdata")

test_that("get_variables_codes", {
  opts <- options()
  on.exit(options(opts))
  options(wqbc.messages = FALSE)

  expect_identical(get_codes(get_variables(c(get_codes(),NA,"KRYP"))),
                   c(get_codes(), NA, NA))
  expect_identical(get_variables(get_codes(c(get_variables(),NA,"Kryptonite"))),
                   c(get_variables(), NA, NA))
})
