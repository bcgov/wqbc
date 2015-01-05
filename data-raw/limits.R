input_limits <- function () {
  require(devtools)

  limits <- read.csv("data-raw/limits.csv", na.strings = c("NA", ""), stringsAsFactors = FALSE)

  stopifnot(identical(colnames(limits),
                      c("Variable", "Jurisdiction",
                        "Use", "SubUse", "Samples", "Days", "Average",
                        "Condition", "Guideline", "Unit",
                        "Comments", "Date", "URL", "TableNumber")))

  stopifnot(identical(sort(unique(limits$Jurisdiction)),
                      c("BC", "CA")))

  stopifnot(identical(sort(unique(limits$Use)),
                      c("Drinking", "Freshwater Life", "Irrigation", "Livestock",
                        "Marine Life", "Recreation", "Wildlife")))

  stopifnot(identical(sort(unique(limits$Unit)),
                      c("/dL", "m", "mg/L", "NTU", "pH", "ug/L")))

  #  limits$Unit <- gsub("µg/L", "ug/L", limits$Unit)
  #  limits$Unit <- gsub(" ", "", limits$Unit)
  #  limits$Date <- gsub("--", "-", limits$Date)
  #  write.csv(limits, "data-raw/limits.csv", row.names = FALSE)

  limits$Date <- as.Date(limits$Date)

  stopifnot(is.integer(limits$Samples))
  stopifnot(is.integer(limits$Days))

  stopifnot(all(!is.na(limits$Variable)))
  stopifnot(all(!is.na(limits$Jurisdiction)))
  stopifnot(all(!is.na(limits$Use)))
  stopifnot(all(!is.na(limits$Samples)))
  stopifnot(all(!is.na(limits$Days)))
  stopifnot(all(!is.na(limits$Average)))
  stopifnot(all(!is.na(limits$Guideline)))
  stopifnot(all(!is.na(limits$Unit)))
  stopifnot(all(!is.na(limits$Date)))
  stopifnot(all(!is.na(limits$URL)))

  codes <- read.csv("data-raw/codes.csv", na.strings = c("NA", ""), stringsAsFactors = FALSE)

  stopifnot(identical(colnames(codes), c("Code", "Variable")))

  n <- nrow(limits)
  limits <- merge(codes, limits, by = "Variable")
  stopifnot(n == nrow(limits))

  code <- limits$Code
  limits$Code <- NULL
  limits <- cbind(data.frame(Code = code), limits)

  limits$Variable <- factor(limits$Variable)
  limits$Code <- factor(limits$Code)

  stopifnot(all(!is.na(limits$Code)))

  limits$Jurisdiction <- factor(limits$Jurisdiction)

  limits$Use <- factor(limits$Use, levels = c(
    "Freshwater Life", "Marine Life", "Drinking", "Livestock",
    "Wildlife", "Irrigation", "Recreation"))

  limits <- limits[!is.na(limits$Variable) &
                             !is.na(limits$Code) &
                             !is.na(limits$Jurisdiction) &
                             !is.na(limits$Use) &
                             !is.na(limits$Sample) &
                             !is.na(limits$Days) &
                             !is.na(limits$Average) &
                             !is.na(limits$Guideline) &
                             !is.na(limits$Unit) &
                             !is.na(limits$Date) &
                             !is.na(limits$URL),,drop = FALSE]

  limits
}
rm(limits)
limits <- input_limits()
summary(limits)
devtools::use_data(limits, overwrite = TRUE, compress = "xz")
