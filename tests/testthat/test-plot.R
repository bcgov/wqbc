context("plot")

test_that("plot_wqis", {
  require(ggplot2)

  data(ccme)
  x <- plot_wqis(calc_wqis(ccme))
  expect_is(x, "gg")
  expect_is(x, "ggplot")
})

test_that("plot_map", {
  require(ggplot2)
  require(sp)
  require(rgdal)

  data(fraser)
  x <- plot_map(fraser)
  expect_is(x, "gg")
  expect_is(x, "ggplot")
})

