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
  expect_is(x,"data.frame")                                                 #check if lookup_limits is a data frame
  expect_equal(nrow(x),23)                                                  #check if lookup_limits has 23 rows
  expect_equal(ncol(x),3)                                                   #check if loopup_limits has 3 columns

  y<-lookup_limits(term="long")
  expect_identical(x,y)                                                     #check if lookup_limits() and lookup_limits(term="long")
  expect_equal(x$UpperLimit[x$Variable=="Arsenic Total"],5)                 #check if Arsenic Total has an upperlimits with 5
  expect_equal(as.character(x$Unit[x$Variable=="Arsenic Total"]),"ug/L")    #check if Arsenic Total has unit ug/L

  z<-lookup_limits(term="long", ph=9)
  expect_equal(z$UpperLimit[z$Variable=="Aluminium Dissolved"],0.050)       #check if Aluminium Dissolved with ph=9 and long term equals to 0.05 upperlimits

  m<-lookup_limits(term="long", ph=6.5)
  expect_equal(z$UpperLimit[z$Variable=="Aluminium Dissolved"],0.050)       #check if Aluminium Dissolved with ph=9 and long term equals to 0.05 upperlimits

  n<-lookup_limits(term="long", ph=5)
  expect_equal(as.character(n$Unit[n$Variable=="Aluminium Dissolved"]),"mg/L")  #check units with different conditions
  expect_equal(n$UpperLimit[n$Variable=="Aluminium Dissolved"],0.006839778)
  expect_equal(as.character(n$UpperLimit[n$Variable=="Cadmium Dissolved"]), as.character(NA)) #upperlimit with NA
  expect_equal(as.character(n$UpperLimit[n$Variable=="Copper Total"]), as.character(NA))
  expect_equal(as.character(n$UpperLimit[n$Variable=="Sulphate"]), as.character(NA))


  q<-lookup_limits(term="short")
  expect_equal(as.character(q$UpperLimit[q$Variable=="Aluminium Dissolved"]),as.character(NA))
  expect_equal(q$UpperLimit[q$Variable=="Polychlorinated Biphenyl Total"],0.10)
  expect_equal(as.character(q$Unit[q$Variable=="Polychlorinated Biphenyl Total"]),"ng/L")

  p<-lookup_limits(term="short",ph=9)
  expect_equal(as.character(p$Unit[p$Variable=="Aluminium Dissolved"]),"mg/L")
  expect_equal(as.character(p$Unit[p$Variable=="Lead"]),"ug/L")
  expect_equal(as.character(p$Unit[p$Variable=="Ethinylestradiol 17a"]),"ng/L")
  expect_equal(p$UpperLimit[p$Variable=="Aluminium Dissolved"],0.10)
  expect_equal(p$UpperLimit[p$Variable=="Cobalt Total"],110.00)
  expect_equal(as.character(p$UpperLimit[p$Variable=="Lead"]),as.character(NA))
  expect_equal(as.character(p$UpperLimit[p$Variable=="Manganese"]),as.character(NA))
  expect_equal(as.character(p$UpperLimit[p$Variable=="Zinc Total"]),as.character(NA)) #NA upperlimits
  #expect_equal(as.character(p$UpperLimit[p$Variable=="zinc Total"]),as.character(NA)) #case sensitive?


  i<-lookup_limits(term="short",ph=6.5)
  expect_equal(as.character(p$Unit[p$Variable=="Aluminium Dissolved"]),"mg/L")


})
