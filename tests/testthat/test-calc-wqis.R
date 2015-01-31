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

test_that("calc_wqis", {
  data(ccme)
  x <- calc_wqis(ccme, messages = FALSE)

  expect_is(x, "data.frame")
  expect_equal(nrow(x), 1)
  expect_equal(colnames(x), c("WQI", "Lower", "Upper", "Category", "Variables", "Tests", "F1", "F2", "F3"))
  expect_equal(x$WQI, 88)
  expect_equal(x$Lower, 87)
  expect_equal(x$Upper, 94)
  expect_equal(as.character(x$Category), "Good")
})

test_that("calc_wqis by", {
  data(ccme)
  x <- calc_wqis(ccme, by = "Date", messages = FALSE)

  expect_is(x, "data.frame")
  expect_equal(nrow(x), 0)
  expect_equal(colnames(x), c("Date", "WQI", "Lower", "Upper", "Category", "Variables", "Tests", "F1", "F2", "F3"))
})
