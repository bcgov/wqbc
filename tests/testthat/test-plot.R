context("plot")

test_that("aes_string_point_expr", {
  head <- "(x = 'Test'"
  tail <- ""
  expect_identical(aes_string_point_expr(head = head, tail = tail, size = 1),
                   paste0("(x = 'Test'), size = 1"))
  expect_identical(aes_string_point_expr(head = head, tail = tail, size = "1"),
                   paste0("(x = 'Test', size = '1')"))
  expect_identical(aes_string_point_expr(head = head, tail = tail, size = 1, shape = 1),
                   paste0("(x = 'Test'), size = 1, shape = 1"))
  expect_identical(aes_string_point_expr(head = head, tail = tail, size = "1", shape = "1"),
                   paste0("(x = 'Test', size = '1', shape = '1')"))
  expect_identical(aes_string_point_expr(head = head, tail = tail, size = 1, shape = "1"),
                   paste0("(x = 'Test', shape = '1'), size = 1"))

  head <- "("
  expect_identical(aes_string_point_expr(head = head, tail = tail, size = 1),
                   paste0("(), size = 1"))
  expect_identical(aes_string_point_expr(head = head, tail = tail, size = "1"),
                   paste0("(size = '1')"))
  expect_identical(aes_string_point_expr(head = head, tail = tail, size = 1, shape = 1),
                   paste0("(), size = 1, shape = 1"))
  expect_identical(aes_string_point_expr(head = head, tail = tail, size = "1", shape = "1"),
                   paste0("(size = '1', shape = '1')"))
  expect_identical(aes_string_point_expr(head = head, tail = tail, size = 1, shape = "1"),
                   paste0("(shape = '1'), size = 1"))
})

test_that("get_category_colours", {
  x <- get_category_colours()
  expect_is(x, "character")
  expect_equal(length(x), 5)
  expect_is(names(x), "character")
  expect_equal(length(names(x)), 5)
  expect_true(all(c("Excellent", "Good", "Fair", "Marginal", "Poor") %in% names(x)))
})

test_that("plot_wqis", {
  require(ggplot2, quietly = TRUE)

  data(ccme)
  x <- plot_wqis(calc_wqi(ccme, messages = FALSE))
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

