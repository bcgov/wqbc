input_limits <- function () {
  require(dplyr)
  require(magrittr)

  limits <- read.csv("data-raw/limits.csv", na.strings = c("NA", ""), stringsAsFactors = FALSE)

  stopifnot(identical(colnames(limits),
                      c("Variable", "Jurisdiction",
                        "Use", "SubUse", "Samples", "Days", "Average",
                        "Condition", "LowerLimit", "UpperLimit", "Units",
                        "Comments", "Date", "URL", "TableNumber")))

  stopifnot(identical(sort(unique(limits$Jurisdiction)),
                      c("BC", "CA")))

  stopifnot(identical(sort(unique(limits$Use)),
                      c("Drinking", "Freshwater Life", "Irrigation", "Livestock",
                        "Marine Life", "Recreation", "Wildlife")))

  stopifnot(identical(sort(unique(limits$Units)),
                      c("/dL", "m", "mg/L", "NTU", "pH", "ug/L")))

  stopifnot(identical(sort(unique(limits$Status)),
                      c("Approved")))

  #    write.csv(limits, "data-raw/limits.csv", row.names = FALSE)

  limits$Date <- as.Date(limits$Date)

  stopifnot(is.integer(limits$Samples))
  stopifnot(is.integer(limits$Days))

  stopifnot(all(!is.na(limits$Variable)))
  stopifnot(all(!is.na(limits$Jurisdiction)))
  stopifnot(all(!is.na(limits$Use)))
  stopifnot(all(!is.na(limits$Samples)))
  stopifnot(all(!is.na(limits$Days)))
  stopifnot(all(!is.na(limits$Average)))
  stopifnot(all(!is.na(limits$LowerLimit) | !is.na(limits$UpperLimit)))
  stopifnot(all(!is.na(limits$Units)))
  stopifnot(all(!is.na(limits$Status)))
  stopifnot(all(!is.na(limits$Date)))
  stopifnot(all(!is.na(limits$URL)))

  codes <- read.csv("data-raw/codes.csv", na.strings = c("NA", ""), stringsAsFactors = FALSE)

  stopifnot(identical(colnames(codes), c("Code", "Variable")))

  stopifnot(all(!is.na(codes$Code)))

  n <- nrow(limits)
  limits <- merge(codes, limits, by = "Variable")
  stopifnot(n == nrow(limits))

  code <- limits$Code
  limits$Code <- NULL
  limits <- cbind(data.frame(Code = code), limits)

  limits$Variable <- factor(limits$Variable)
  limits$Code <- factor(limits$Code)
  limits$Status <- factor(limits$Code, levels = c("Approved"))

  limits$Jurisdiction <- factor(limits$Jurisdiction, levels = c("BC", "CA"))

  limits$Use <- factor(limits$Use, levels = c(
    "Freshwater Life", "Marine Life", "Drinking", "Livestock",
    "Wildlife", "Irrigation", "Recreation"))

  limits %<>% dplyr::filter(
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

  limits %<>% select(Code, Variable, Jurisdiction, Use, SubUse,
                     Samples, Days,
                     Average, Condition, LowerLimit, UpperLimit, Units)
  limits
}
require(devtools)
rm(limits)
limits <- input_limits()
summary(limits)
devtools::use_data(limits, overwrite = TRUE, compress = "xz")
