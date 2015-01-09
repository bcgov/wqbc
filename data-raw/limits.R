library(dplyr)
library(magrittr)
library(devtools)

rm(list = ls())
graphics.off()

reassign_aquatic_life <- function (x) {
  x2 <- filter_(x, ~Use == "Aquatic Life")
  x2$Use <- "Marine Life"
  x$Use <- sub("Aquatic Life", "Freshwater Life", x$Use)
  x <- rbind(x, x2)
  x
}

set_periods <- function (x) {
  x <- dplyr::rename(x, Period = Days)
  x$Period <- factor(x$Period)
  levels(x$Period) <- list(Day = "1", Month = "30")
  x
}

check_valid_expression <- function (x) {
  parse(text = x)
  TRUE
}

read_limits <- function () {
  read.csv("data-raw/limits.csv", na.strings = c("NA", ""), stringsAsFactors = FALSE)
}

read_codes <- function () {
  read.csv("data-raw/codes.csv", na.strings = c("NA", ""), stringsAsFactors = FALSE)
}

check_codes <- function () {

  x <- read_codes()

  stopifnot(identical(colnames(x), c("Code", "Variable")))
  stopifnot(all(!is.na(x$Code)))
  stopifnot(all(!is.na(x$Variable)))

  TRUE
}

check_limits <- function () {

  x <- read_limits()

  stopifnot(identical(colnames(x),
                      c("Variable", "Jurisdiction",
                        "Use", "SubUse", "Samples", "Days", "Average",
                        "Condition", "LowerLimit", "UpperLimit", "Units",
                        "Status", "Comments", "URL", "TableNumber")))

  stopifnot(identical(sort(unique(x$Jurisdiction)),
                      c("BC", "CA")))

  stopifnot(identical(sort(unique(x$Use)),
                      c("Aquatic Life", "Drinking", "Freshwater Life",
                        "Industrial", "Irrigation", "Livestock",
                        "Marine Life", "Recreation", "Wildlife")))

  stopifnot(all.equal(sort(unique(x$Days)),
                      c(1, 30)))

  stopifnot(identical(sort(unique(x$Average)),
                      c("geomean1", "max", "mean", "median")))

  stopifnot(identical(sort(unique(x$Units)),
                      c("/dL", "m", "mg/L", "NTU", "pH", "ug/L")))

  stopifnot(identical(sort(unique(x$Status)),
                      c("Approved")))

  stopifnot(is.integer(x$Samples))
  stopifnot(is.integer(x$Days))

  check_valid_expression(x$Condition)
  check_valid_expression(x$LowerLimit)
  check_valid_expression(x$UpperLimit)

  stopifnot(all(!is.na(x$Variable)))
  stopifnot(all(!is.na(x$Jurisdiction)))
  stopifnot(all(!is.na(x$Use)))
  stopifnot(all(!is.na(x$Samples)))
  stopifnot(all(!is.na(x$Days)))
  stopifnot(all(!is.na(x$Average)))
  stopifnot(all(!is.na(x$LowerLimit) | !is.na(x$UpperLimit)))
  stopifnot(all(!is.na(x$Units)))

  TRUE
}

input_limits <- function () {

  require(dplyr)
  require(magrittr)

  check_limits()

  limits <- read_limits()

  unique(select(limits, Jurisdiction, URL))

  lapply(limits, FUN = function (limits) (sort(unique(limits))))
  # write.csv(limits, "data-raw/limits.csv", row.names = FALSE)

  limits %<>% reassign_aquatic_life()
  limits %<>% set_periods()

  limits$Status %<>% factor(levels = c("Approved"))
  limits$Jurisdiction %<>% factor(levels = c("BC", "CA"))
  limits$Units %<>% factor(levels = c("ug/L", "mg/L", "g/L", "kg/L", "/dL", "pH", "NTU", "m"))
  limits$Average %<>% factor(levels = c("geomean1", "max", "mean", "median"))
  limits$Use %<>% factor(levels = c(
    "Freshwater Life", "Marine Life", "Drinking", "Livestock",
    "Wildlife", "Irrigation", "Recreation", "Industrial"))

  check_codes()
  codes <- read_codes()

  limits <- inner_join(codes, limits, by = "Variable")

  # move code to first column position
  code <- limits$Code
  limits$Code <- NULL
  limits <- cbind(data.frame(Code = code), limits)

  limits %<>% dplyr::filter(
    !is.na(Variable) &
      !is.na(Code) &
      !is.na(Jurisdiction) &
      !is.na(Use) &
      !is.na(Samples) &
      !is.na(Period) &
      !is.na(Average) &
      !(is.na(LowerLimit) & is.na(UpperLimit)) &
      !is.na(Units) &
      Status == "Approved")

  limits %<>% arrange(Code, Use, SubUse, Jurisdiction, Samples, Period)

  limits %<>% select(Code, Variable, Jurisdiction, Use, SubUse,
                     Samples, Period,
                     Average, Condition, LowerLimit, UpperLimit, Units)

  limits$Code %<>% factor
  limits$Variable %<>% factor
  limits$Jurisdiction %<>% droplevels
  limits$Average %<>% droplevels

  limits
}
limits <- input_limits()
summary(limits)
# devtools::use_data(limits, overwrite = TRUE, compress = "xz")
