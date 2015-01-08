library(dplyr)
library(magrittr)
library(devtools)

rm(list = ls())
graphics.off()

reassign_aquatic_life <- function (x) {
  x2 <- filter(x, Use == "Aquatic Life")
  x2$Use <- "Marine Life"
  x$Use <- sub("Aquatic Life", "Freshwater Life", x$Use)
  x <- rbind(x, x2)
  x
}

input_limits <- function () {
  require(dplyr)
  require(magrittr)

  x <- read.csv("data-raw/limits.csv", na.strings = c("NA", ""), stringsAsFactors = FALSE)

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

  stopifnot(identical(sort(unique(x$Units)),
                      c("/dL", "m", "mg/L", "NTU", "pH", "ug/L")))

  stopifnot(identical(sort(unique(x$Status)),
                      c("Approved")))

  #  write.csv(x, "data-raw/limits.csv", row.names = FALSE)

  x %<>% reassign_aquatic_life()
  x$Date %<>% as.Date

  x$Status %<>% factor(levels = c("Approved"))

  x$Jurisdiction %<>% factor(levels = c("BC", "CA"))

  x$Use %<>% factor(levels = c(
    "Freshwater Life", "Marine Life", "Drinking", "Livestock",
    "Wildlife", "Irrigation", "Recreation", "Industrial"))

  stopifnot(is.integer(x$Samples))
  stopifnot(is.integer(x$Days))

  stopifnot(all(!is.na(x$Variable)))
  stopifnot(all(!is.na(x$Jurisdiction)))
  stopifnot(all(!is.na(x$Use)))
  stopifnot(all(!is.na(x$Samples)))
  stopifnot(all(!is.na(x$Days)))
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
