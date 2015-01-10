context("plot")

test_that("plot_wqis", {
  require(ggplot2, quietly = TRUE)

  data(ccme)
  x <- plot_wqis(calc_wqis(ccme))
  expect_is(x, "gg")
  expect_is(x, "ggplot")
})

test_that("plot_map", {
  require(ggplot2, quietly = TRUE)
  require(sp, quietly = TRUE)
  require(rgdal, quietly = TRUE)

  data(fraser)
  x <- plot_map(fraser)
  expect_is(x, "gg")
  expect_is(x, "ggplot")
})

