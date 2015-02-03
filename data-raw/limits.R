library(wqbc)
library(dplyr)
library(magrittr)
library(devtools)

rm(list = ls())

input_limits <- function () {

  limits <- read.csv("data-raw/limits.csv", na.strings = c("NA", ""), stringsAsFactors = FALSE)

  stopifnot(nrow(limits) == 67)

  stopifnot(identical(colnames(limits),
                      c("Variable", "Term",
                        "Condition", "UpperLimit", "Units", "Table",
                        "Reference", "Use")))

  stopifnot(all(!is.na(select(limits, -Condition))))

  stopifnot(all(limits$Term %in% c("Short", "Long")))
  stopifnot(all(limits$Units %in% get_units()))
  stopifnot(all(limits$Reference %in% c("BC_2006", "EMAIL_2014", "EMAIL_2015")))
  stopifnot(all(limits$Use %in% c("Freshwater Life")))

  check_valid_expression <- function (x) {
    parse(text = x)
    TRUE
  }

  check_valid_expression(limits$Condition)
  check_valid_expression(limits$UpperLimit)

  limits <- rename_(limits, "..Units" = "Units")

  load("data/codes.rda")

  limits$Variable <- factor(limits$Variable, levels = levels(codes$Variable))
  stopifnot(all(!is.na(limits$Variable)))

  codes$Code <- wqbc:::strip_ems_codes(codes$Code)

  limits <- inner_join(limits, codes, by = "Variable")

  stopifnot(all(limits$..Units == limits$Units))
  limits$..Units <- NULL

  limits %<>% arrange(Variable, Term)

  limits %<>% select(Variable, Term, Condition, UpperLimit, Units)

  limits$Term %<>% factor
  limits$Units %<>% droplevels
  limits
}
limits <- input_limits()
use_data(limits, overwrite = TRUE, compress = "xz")
