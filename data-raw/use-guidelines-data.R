input_guidelines <- function () {
  require(devtools)

  guidelines <- read.csv("data-raw/guidelines.csv", na.strings = c("NA", ""), stringsAsFactors = FALSE)

  stopifnot(identical(colnames(guidelines),
                      c("Variable", "Jurisdiction",
                        "Use", "SubUse", "Samples", "Days", "Average",
                        "Condition", "Guideline", "Unit",
                        "Comments", "Date", "URL", "TableNumber")))

  stopifnot(identical(sort(unique(guidelines$Jurisdiction)),
                      c("BC", "CA")))

  stopifnot(identical(sort(unique(guidelines$Use)),
                      c("Drinking", "Freshwater Life", "Irrigation", "Livestock",
                        "Marine Life", "Recreation", "Wildlife")))

  stopifnot(identical(sort(unique(guidelines$Unit)),
                      c("/dL", "m", "mg/L", "NTU", "pH", "ug/L")))

  #  guidelines$Unit <- gsub("µg/L", "ug/L", guidelines$Unit)
  #  guidelines$Unit <- gsub(" ", "", guidelines$Unit)
  #  guidelines$Date <- gsub("--", "-", guidelines$Date)
  #  write.csv(guidelines, "data-raw/guidelines.csv", row.names = FALSE)

  guidelines$Date <- as.Date(guidelines$Date)

  stopifnot(is.integer(guidelines$Samples))
  stopifnot(is.integer(guidelines$Days))

  stopifnot(all(!is.na(guidelines$Variable)))
  stopifnot(all(!is.na(guidelines$Jurisdiction)))
  stopifnot(all(!is.na(guidelines$Use)))
  stopifnot(all(!is.na(guidelines$Samples)))
  stopifnot(all(!is.na(guidelines$Days)))
  stopifnot(all(!is.na(guidelines$Average)))
  stopifnot(all(!is.na(guidelines$Guideline)))
  stopifnot(all(!is.na(guidelines$Unit)))
  stopifnot(all(!is.na(guidelines$Date)))
  stopifnot(all(!is.na(guidelines$URL)))

  variables <- read.csv("data-raw/variables.csv", na.strings = c("NA", ""), stringsAsFactors = FALSE)

  stopifnot(identical(colnames(variables), c("Code", "Variable")))

  n <- nrow(guidelines)
  guidelines <- merge(variables, guidelines, by = "Variable")
  stopifnot(n == nrow(guidelines))

  guidelines$Variable <- factor(guidelines$Variable)
  guidelines$Code <- factor(guidelines$Code)

  stopifnot(all(!is.na(guidelines$Code)))

  guidelines$Jurisdiction <- factor(guidelines$Jurisdiction)

  guidelines$Use <- factor(guidelines$Use, levels = c(
    "Freshwater Life", "Marine Life", "Drinking", "Livestock",
    "Wildlife", "Irrigation", "Recreation"))

  guidelines <- guidelines[!is.na(guidelines$Variable) &
                             !is.na(guidelines$Code) &
                             !is.na(guidelines$Jurisdiction) &
                             !is.na(guidelines$Use) &
                             !is.na(guidelines$Sample) &
                             !is.na(guidelines$Days) &
                             !is.na(guidelines$Average) &
                             !is.na(guidelines$Guideline) &
                             !is.na(guidelines$Unit) &
                             !is.na(guidelines$Date) &
                             !is.na(guidelines$URL),,drop = FALSE]

  guidelines
}
rm(guidelines)
guidelines <- input_guidelines()
summary(guidelines)
devtools::use_data(guidelines, overwrite = TRUE, compress = "xz")
