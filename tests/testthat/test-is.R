library(wqbc)
context("is")

test_that("is wq_thresholds", {
  x <- data.frame()
  class(x) <- c("wq_thresholds", "data.frame")
  expect_true(is_wq_thresholds(x))
  expect_false(is_wq_index(x))
})
