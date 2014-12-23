# percent of failed variables
F1 <- function (x) {
  nfv <- 0   # number of failed variables
  nfv / length(unique(x$Variable)) * 100
}

# percentage of failed tests
F2 <- function (x) {
  length(sum(x$Failed)) / nrow(x) * 100
}

# the amount by which failed test values do not meet their objectives
F3 <- function (x) {
  x <- x[x$Failed,,drop = FALSE]

  if(nrow(x) == 0)
    return (0)

  bol <- TRUE # need to fix this up
  excursion1 <- x$Value[bol] / x$Guideline[bol]
  excursion2 <- x$Guideline[!bol] / x$Value[!bol]

  nse <-  sum(excursion1, excursion2) / nrow(x)
  nse / (0.01 * nse + 0.01)
}

# categorize_wqi(1:100)

categorize_wqi <- function (x) {
  cut(x, breaks = c(-1, 44, 64, 79, 94, 100),
      labels = c("Poor", "Marginal", "Fair", "Good", "Excellent"),
      ordered_result = TRUE)
}

#data <- data.frame(Variable = c("As", "As", "Al-T"), Value = c(4,5,6), Guideline = 5, Condition "<=")

calc_wqi <- function (x) {
  # can likely remove once set up higher level
  assert_that(is.data.frame(x))
  assert_that(is.factor(x$Variable) && noNA(x$Variable))
  assert_that(is.numeric(x$Value) && noNA(x$Value))
  assert_that(is.numeric(x$Guideline) && noNA(x$Guideline))
  assert_that(is.character(x$Condition) && noNA(x$Condition))
  assert_that(is.character(x$Condition) && noNA(x$Condition))
  assert_that(all(x$Condition %in% c("<", "<=", ">", ">=")))

  # determine if test failed or not.. x$Failed <-
  F1 <- F1(x)
  F2 <- F2(x)
  F3 <- F3(x)
  WQI <- 100 - sqrt(F1^2 + F2^2 + F3^2) / 1.732
  Category <- categorize_wqi(WQI)
  data.frame(WQI = round(WQI), Category = Category,
             Variables = length(unique(x$Variable)), Tests = nrow(x),
             F1 = signif(F1, 3), F2 = signif(F2, 3), F3 = signif(F3, 3))
}

calc_wqis <- function (x, by = NULL) {
  # might need to strip out non-constant values....
  if(is.null(by)) {
    return(calc_wqi(x))
  }
  plyr::ddply(x, .variables = by, .fun = calc_wqi, .parallel = TRUE)
}
