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
test_that("lookup_limits", {
  x<-lookup_limits()
  expect_is(x,"data.frame")
  expect_equal(nrow(x),23)
  expect_equal(ncol(x),3)
  y<-lookup_limits(term="long")

  expect_identical(x,y)
  expect_equal(x$UpperLimit[x$Variable=="Arsenic Total"],5)
  expect_equal(as.character(x$Unit[x$Variable=="Arsenic Total"]),"ug/L")
  z<-lookup_limits(term="long", ph=9)

  expect_equal(z$UpperLimit[z$Variable=="Aluminium Dissolved"],0.050)
})
