context("lookup")

test_that("get_variables", {
  expect_is(get_variables(), "character")
  expect_identical(get_variables(c("Ag", "KR", "As", NA, "pH", "TP")),
                   c("Silver", NA, "Arsenic", NA, "pH", "Total Phosphorus"))

})

test_that("get_codes", {
  expect_is(get_codes(), "character")
  expect_identical(get_codes(c("Silver", "Kryptonite", "Arsenic", NA, "pH",
                               "Total Phosphorus")),
                   c("Ag", NA, "As", NA, "pH", "TP"))
})

test_that("get_category_colours", {
  x <- get_category_colours()
  expect_is(x, "character")
  expect_equal(length(x), 5)
  expect_is(names(x), "character")
  expect_equal(length(names(x)), 5)
  expect_true(all(c("Excellent", "Good", "Fair", "Marginal", "Poor") %in% names(x)))
})
