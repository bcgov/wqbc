context("tidyr")

test_that("spread_", {
  x <- data.frame(Date = as.Date("2000-01-01"), Key = c(1,1), Value = 1:2)
  expect_error(tidyr::spread_(x, "Key", "Value"))
})
