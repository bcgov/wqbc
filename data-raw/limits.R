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

check_limits <- function (x) {
  stopifnot(identical(colnames(x),
                      c("Variable", "Jurisdiction",
                        "Use", "SubUse", "Samples", "Days", "Average",
                        "Condition", "LowerLimit", "UpperLimit", "Units",
                        "Status", "Comments", "Date", "URL", "TableNumber")))

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

  stopifnot(is.integer(x$Days))

  check_valid_expression(x$Condition)
  check_valid_expression(x$LowerLimit)
  check_valid_expression(x$UpperLimit)

  TRUE
}

input_limits <- function () {
  require(dplyr)
  require(magrittr)

  x <- read.csv("data-raw/limits.csv", na.strings = c("NA", ""), stringsAsFactors = FALSE)

  check_limits(x)

  lapply(x, FUN = function (x) (sort(unique(x))))


 # write.csv(x, "data-raw/limits.csv", row.names = FALSE)

  x %<>% reassign_aquatic_life()
  x %<>% set_periods()

  x$Date %<>% as.Date

  x$Status %<>% factor(levels = c("Approved"))

  x$Jurisdiction %<>% factor(levels = c("BC", "CA"))

  x$Units %<>% factor(levels = c("ug/L", "mg/L", "/dL", "pH", "NTU", "m"))

  x$Average %<>% factor(levels = c("geomean1", "max", "mean", "median"))

  x$Use %<>% factor(levels = c(
    "Freshwater Life", "Marine Life", "Drinking", "Livestock",
    "Wildlife", "Irrigation", "Recreation", "Industrial"))

sort(unique(x$UpperLimit))

  stopifnot(is.integer(x$Samples))

  stopifnot(all(!is.na(x$Variable)))
  stopifnot(all(!is.na(x$Jurisdiction)))
  stopifnot(all(!is.na(x$Use)))
  stopifnot(all(!is.na(x$Samples)))
  stopifnot(all(!is.na(x$Period)))
  stopifnot(all(!is.na(x$Average)))
  stopifnot(all(!is.na(x$LowerLimit) | !is.na(x$UpperLimit)))
  stopifnot(all(!is.na(x$Units)))
  stopifnot(all(!is.na(x$Status)))
  stopifnot(all(!is.na(x$Date)))
  stopifnot(all(!is.na(x$URL)))

  codes <- read.csv("data-raw/codes.csv", na.strings = c("NA", ""), stringsAsFactors = FALSE)

  stopifnot(identical(colnames(codes), c("Code", "Variable")))

  stopifnot(all(!is.na(codes$Code)))

  n <- nrow(x)
  x <- merge(codes, x, by = "Variable")
  stopifnot(n == nrow(x))

  code <- x$Code
  x$Code <- NULL
  x <- cbind(data.frame(Code = code), x)

  x$Variable %<>% factor
  x$Code %<>% factor

  x %<>% dplyr::filter(
    !is.na(Variable) &
      !is.na(Code) &
      !is.na(Jurisdiction) &
      !is.na(Use) &
      !is.na(Samples) &
      !is.na(Days) &
      !is.na(Average) &
      !(is.na(LowerLimit) & is.na(UpperLimit)) &
      !is.na(Units) &
      #  Status == "Approved" &
      !is.na(Date) &
      !is.na(URL))

  x %<>% arrange(Code, Use, SubUse, Jurisdiction, Samples, Days)

  x %<>% select(Code, Variable, Jurisdiction, Use, SubUse,
                     Samples, Days,
                     Average, Condition, LowerLimit, UpperLimit, Units)
  x
}
limits <- input_limits()
summary(limits)
devtools::use_data(limits, overwrite = TRUE, compress = "xz")
