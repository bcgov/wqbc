context("calc-wqis")

test_that("categorize_wqi ", {
  expect_true(is.na(as.character(categorize_wqi(-1))))
  expect_equal(as.character(categorize_wqi(0)), "Poor")
  expect_equal(as.character(categorize_wqi(44)), "Poor")
  expect_equal(as.character(categorize_wqi(45)), "Marginal")
  expect_equal(as.character(categorize_wqi(64)), "Marginal")
  expect_equal(as.character(categorize_wqi(65)), "Fair")
  expect_equal(as.character(categorize_wqi(79)), "Fair")
  expect_equal(as.character(categorize_wqi(80)), "Good")
  expect_equal(as.character(categorize_wqi(94)), "Good")
  expect_equal(as.character(categorize_wqi(95)), "Excellent")
  expect_equal(as.character(categorize_wqi(100)), "Excellent")
  expect_true(is.na(as.character(categorize_wqi(101))))
})

test_that("ccme", {
  opts <- options()
  on.exit(options(opts))
  options(wqbc.messages = FALSE)

  data(ccme)

  x <- calc_wqis(ccme, messages = FALSE)

  expect_is(x, "data.frame")
  expect_equal(nrow(x), 1)
  expect_equal(colnames(x), c("WQI", "Lower", "Upper", "Category", "Variables", "Tests", "F1", "F2", "F3"))
  expect_equal(round(x$WQI), 88)
  expect_equal(round(x$Lower), 87)
  expect_equal(round(x$Upper), 94)
  expect_equal(as.character(x$Category), "Good")

  is.na(ccme$Value[ccme$Variable == "Dissolved Oxygen" & ccme$Date == as.Date("1994-03-04")]) <- TRUE
  x <- calc_wqis(ccme)
  expect_equal(x$Variables, 10)
  expect_equal(x$Tests, 102)
  expect_equal(x$WQI, 88.1)
  is.na(ccme$Value[ccme$Variable == "Arsenic"]) <- TRUE
  x <- calc_wqis(ccme)
  expect_equal(x$Variables, 9)
  expect_equal(x$Tests, 90)
  expect_equal(x$WQI, 86.8)
  is.na(ccme$Value[ccme$Variable == "Arsenic"]) <- TRUE
  ccme$Value[ccme$Variable == "Lindane"] <- 0
  x <- calc_wqis(ccme)
  expect_equal(x$Variables, 9)
  expect_equal(x$Tests, 90)
  expect_equal(x$WQI, 86.8)

  ccme$Value[ccme$Variable == "Dissolved Oxygen" & ccme$Date == as.Date("1997-04-08")] <- 0
  expect_error(calc_wqis(ccme))
})

test_that("calc_wqis by", {
  data(ccme)
  x <- calc_wqis(ccme, by = "Date", messages = FALSE)

  expect_is(x, "data.frame")
  expect_equal(nrow(x), 0)
  expect_equal(colnames(x), c("Date", "WQI", "Lower", "Upper", "Category", "Variables", "Tests", "F1", "F2", "F3"))
})
