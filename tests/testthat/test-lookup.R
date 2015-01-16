context("lookup")

test_that("get_variables_codes", {
  expect_identical(get_codes(get_variables(c(get_codes(),NA,"KR"))),
                   c(get_codes(), NA, NA))
  expect_identical(get_variables(get_codes(c(get_variables(),NA,"Kryptonite"))),
                   c(get_variables(), NA, NA))
})

test_that("get_category_colours", {
  x <- get_category_colours()
  expect_is(x, "character")
  expect_equal(length(x), 5)
  expect_is(names(x), "character")
  expect_equal(length(names(x)), 5)
  expect_true(all(c("Excellent", "Good", "Fair", "Marginal", "Poor") %in% names(x)))
})
