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

  z<-lookup_limits(term="long", ph=9) #EMS_0004
  expect_equal(z$UpperLimit[z$Variable=="Aluminium Dissolved"],0.050)       #check if Aluminium Dissolved with ph=9 and long term equals to 0.05 upperlimits

  m<-lookup_limits(term="long", ph=6.5)#EMS_0004
  expect_equal(z$UpperLimit[z$Variable=="Aluminium Dissolved"],0.050)       #check if Aluminium Dissolved with ph=9 and long term equals to 0.05 upperlimits

  n<-lookup_limits(term="long", ph=5)#EMS_0004
  expect_equal(as.character(n$Unit[n$Variable=="Aluminium Dissolved"]),"mg/L")  #check units with different conditions
  expect_equal(n$UpperLimit[n$Variable=="Aluminium Dissolved"],0.006839778)
  expect_equal(as.character(n$UpperLimit[n$Variable=="Cadmium Dissolved"]), as.character(NA)) #upperlimit with NA
  expect_equal(as.character(n$UpperLimit[n$Variable=="Copper Total"]), as.character(NA))
  expect_equal(as.character(n$UpperLimit[n$Variable=="Sulphate"]), as.character(NA))


  q<-lookup_limits(term="short")
  expect_equal(as.character(q$UpperLimit[q$Variable=="Aluminium Dissolved"]),as.character(NA))
  expect_equal(q$UpperLimit[q$Variable=="Polychlorinated Biphenyl Total"],0.10)
  expect_equal(as.character(q$Unit[q$Variable=="Polychlorinated Biphenyl Total"]),"ng/L")

  p<-lookup_limits(term="short",ph=9)#EMS_0004
  expect_equal(as.character(p$Unit[p$Variable=="Aluminium Dissolved"]),"mg/L")
  expect_equal(as.character(p$Unit[p$Variable=="Lead"]),"ug/L")
  expect_equal(as.character(p$Unit[p$Variable=="Ethinylestradiol 17a"]),"ng/L")
  expect_equal(p$UpperLimit[p$Variable=="Aluminium Dissolved"],0.10)
  expect_equal(p$UpperLimit[p$Variable=="Cobalt Total"],110.00)
  expect_equal(as.character(p$UpperLimit[p$Variable=="Lead"]),as.character(NA))
  expect_equal(as.character(p$UpperLimit[p$Variable=="Manganese"]),as.character(NA))
  expect_equal(as.character(p$UpperLimit[p$Variable=="Zinc Total"]),as.character(NA)) #NA upperlimits


  i<-lookup_limits(term="short",ph=6.5)#EMS_0004
  expect_equal(as.character(i$Unit[i$Variable=="Aluminium Dissolved"]),"mg/L")
  expect_equal(as.character(i$Unit[i$Variable=="Cyanide Weak Acid Dissociable"]),"ug/L")
  expect_equal(as.character(i$Unit[i$Variable=="Polychlorinated Biphenyl Total"]),"ng/L")
  expect_equal(i$UpperLimit[i$Variable=="Aluminium Dissolved"],0.1)
  expect_equal(as.character(i$UpperLimit[i$Variable=="Copper Total"]),as.character(NA))
  expect_equal(as.character(i$UpperLimit[i$Variable=="Silver"]),as.character(NA))
  expect_equal(as.character(i$UpperLimit[i$Variable=="Zinc Total"]),as.character(NA))

  #k<-lookup_limits(term="long",Hardness Total=1.3)                             #out of bound
  #exp(0.736 * log(EMS_0107) - 4.943)
  #expect_equal(as.character(k$Unit[k$Variable=="Cadmium Dissolved"]),as.character(NA))
  #expect_equal(as.character(k$UpperLimit[k$Variable=="Cadmium Dissolved"]),as.character(NA))


  #a<-lookup_limits(term="long",Hardness Total=3.4)                             #boundary case for Cadmium Dissolved
  #exp(0.736 * log(EMS_0107) - 4.943)
  #expect_equal(as.character(a$Unit[a$Variable=="Cadmium Dissolved"]),"ug/L")
  #expect_equal(a$UpperLimit[a$Variable=="Cadmium Dissolved"]),0.0105)


  #b<-lookup_limits(term="long",Hardness Total=100)
  #expect_equal(as.character(b$Unit[b$Variable=="Cadmium Dissolved"]),"ug/L")
  #exp(0.736 * log(EMS_0107) - 4.943)
  #expect_equal(b$UpperLimit[b$Variable=="Cadmium Dissolved"]),0.03109)

  #c<-lookup_limits(term="long",Hardness Total=285)                             #boundary case for Cadmium Dissolved
  #exp(0.736 * log(EMS_0107) - 4.943)
  #expect_equal(as.character(c$Unit[c$Variable=="Cadmium Dissolved"]),"ug/L")
  #expect_equal(c$UpperLimit[c$Variable=="Cadmium Dissolved"]),0.043446)

  #d<-lookup_limits(term="long",Hardness Total=300)                             #out of bound
  #exp(0.736 * log(EMS_0107) - 4.943)
  #expect_equal(as.character(d$Unit[d$Variable=="Cadmium Dissolved"]),as.character(NA))
  #expect_equal(as.character(d$UpperLimit[d$Variable=="Cadmium Dissolved"]),as.character(NA))

  #Cadmium<-lookup_limits(term="short",Hardness Total=3)                             #out of bound
  #expect_equal(as.character(Cadmium$Unit[Cadmium$Variable=="Cadmium Dissolved"]),as.character(NA))
  #expect_equal(as.character(Cadmium$UpperLimit[Cadmium$Variable=="Cadmium Dissolved"]),as.character(NA))

  #Cadmium_lowerboundary<-lookup_limits(term="short",Hardness Total=7)                     #boundary for Cadmium Dissolved
  # exp(1.03 * log(EMS_0107) - 5.274)
  #expect_equal(as.character(Cadmium_boundary$Unit[Cadmium_boundary$Variable=="Cadmium Dissolved"]),"ug/L")
  #expect_equal(as.character(Cadmium_boundary$UpperLimit[Cadmium_boundary$Variable=="Cadmium Dissolved"]),0.0122338)

  #Cadmium_range<-lookup_limits(term="short",Hardness Total=300)
  # exp(1.03 * log(EMS_0107) - 5.274)
  #expect_equal(as.character(Cadmium_range$Unit[Cadmium_range$Variable=="Cadmium Dissolved"]),"ug/L")
  #expect_equal(as.character(Cadmium_range$UpperLimit[Cadmium_range$Variable=="Cadmium Dissolved"]),0.0657)

  #Cadmium_upperbound<-lookup_limits(term="short",Hardness Total=455)                     #boundary for Cadmium Dissolved
  # exp(1.03 * log(EMS_0107) - 5.274)
  #expect_equal(as.character(Cadmium_upperbound$Unit[Cadmium_upperbound$Variable=="Cadmium Dissolved"]),"ug/L")
  #expect_equal(as.character(Cadmium_upperbound$UpperLimit[Cadmium_upperbound$Variable=="Cadmium Dissolved"]),0.07916)

  #e<-lookup_limits(term="long",Hardness Total=30)
  # 0.04 * EMS_0107
  #expect_equal(as.character(e$Unit[e$Variable=="Copper Total"]),"ug/L")
  #expect_equal(as.character(e$UpperLimit[e$Variable=="Copper Total"]),2)

  #e<-lookup_limits(term="long",Hardness Total=50)
  # 0.04 * EMS_0107
  #expect_equal(as.character(e$Unit[e$Variable=="Copper Total"]),"ug/L")
  #expect_equal(as.character(e$UpperLimit[e$Variable=="Copper Total"]),2)

  #f<-lookup_limits(term="long",Hardness Total=51)                             # boundary case for Copper Total
  # 0.04 * EMS_0107
  #expect_equal(as.character(f$Unit[f$Variable=="Copper Total"]), "ug/L")
  #expect_equal(as.character(f$UpperLimit[f$Variable=="Copper Total"]),2.04)

  #g<-lookup_limits(term="long",Hardness Total=10000)
  # 0.04 * EMS_0107
  #expect_equal(as.character(e$Unit[e$Variable=="Copper Total"]), "ug/L")
  #expect_equal(as.character(e$UpperLimit[e$Variable=="Copper Total"]),400)

  # FluorideTotal<-lookup_limits(term="short",Hardness Total=-1)               #negative hardness total
  #expect_equal(as.character(FluorideTotal$Unit[FluorideTotal$Variable=="Fluoride Total"]), as.character(NA))
  #expect_equal(as.character(FluorideTotal$UpperLimit[FluorideTotal$Variable=="Fluoride Total"]),as.character(NA))

  # FluorideTotal_inrange<-lookup_limits(term="short",Hardness Total=10)               #boundary
  #expect_equal(as.character(FluorideTotal_inrange$Unit[FluorideTotal_inrange$Variable=="Fluoride Total"]), "mg/L")
  #expect_equal(as.character(FluorideTotal_inrange$UpperLimit[FluorideTotal_inrange$Variable=="Fluoride Total"]),0.4)

  #FluorideTotal_morethan10<-lookup_limits(term="short",Hardness Total=30)
  #(92.57 * log(EMS_0107, base = 10) - 51.73) * 0.01
  #expect_equal(as.character(FluorideTotal_morethan10$Unit[FluorideTotal_morethan10$Variable=="Fluoride Total"]), "mg/L")
  #expect_equal(as.character(FluorideTotal_morethan10$UpperLimit[FluorideTotal_morethan10$Variable=="Fluoride Total"]),0.85)


})
