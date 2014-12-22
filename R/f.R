
#data <- data.frame(Value = c(4,5,6), Guideline = 5, Condition "<=")

F1 <- function (x) {
  assert_that(is.data.frame(x))
  assert_that(is.numeric(x$Value) && noNA(x$Value))
  assert_that(is.numeric(x$Guideline) && noNA(x$Guideline))
  assert_that(is.character(x$Condition) && noNA(x$Condition))
  assert_that(is.character(x$Condition) && noNA(x$Condition))

}
