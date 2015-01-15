library(wqbc)
library(dplyr)

check_limits <- function (x) {

  check_valid_expression <- function (x) {
    parse(text = x)
    TRUE
  }

  stopifnot(identical(colnames(x),
                      c("Variable", "Period",
                        "Condition", "LowerLimit", "UpperLimit", "Units", "Table",
                        "Reference", "Use")))

  stopifnot(all(!is.na(x$Variable)))
  stopifnot(all(!is.na(x$LowerLimit) | !is.na(x$UpperLimit)))
  stopifnot(all(!is.na(x$Units)))
  stopifnot(all(!is.na(x$Table)))
  stopifnot(all(!is.na(x$Reference)))
  stopifnot(all(!is.na(x$Use)))

  stopifnot(all(x$Period[!is.na(x$Period)] %in% c("month")))
  stopifnot(all(x$Units %in% get_units()))
  stopifnot(all(x$Reference %in% c("BC_2006", "EMAIL_2014")))
  stopifnot(all(x$Use %in% c("Freshwater Life")))

  check_valid_expression(x$Condition)
  check_valid_expression(x$LowerLimit)
  check_valid_expression(x$UpperLimit)

  TRUE
}

input_limits <- function () {

  limits <- read.csv("data-raw/limits.csv", na.strings = c("NA", ""), stringsAsFactors = FALSE)

  check_limits(limits)

  # lapply(limits, FUN = function (limits) (sort(unique(limits))))
  # limits %<>% arrange(Variable, Period, UpperLimit)
  #   write.csv(limits, "data-raw/limits.csv", row.names = FALSE, na = "")

  limits <- rename_(limits, "..Units" = "Units")

  load("data/codes.rda")

  n <- nrow(limits)
  limits <- left_join(limits, codes, by = "Variable")
  stopifnot(n == nrow(limits))

  stopifnot(all(limits$..Units == limits$Units))
  limits$..Unit <- NULL

  limits %<>% dplyr::filter(
    !is.na(Variable) &
      !is.na(Code) &
      !(is.na(LowerLimit) & is.na(UpperLimit)) &
      !is.na(Units))

  limits %<>% arrange(Variable, Period)

  limits %<>% select(Variable, Period, Condition, LowerLimit, UpperLimit, Units)

  limits$Period[is.na(limits$Period)] <- "day"
  limits$Period %<>% factor
  limits$Variable %<>% factor
  limits$Units %<>% droplevels

  limits
}
limits <- input_limits()
devtools::use_data(limits, overwrite = TRUE, compress = "xz")
