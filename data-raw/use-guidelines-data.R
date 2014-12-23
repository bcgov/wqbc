input_guidelines <- function () {
  require(devtools)

  variables <- read.csv("data-raw/variables.csv", na.strings = c("NA", ""), stringsAsFactors = TRUE)
  guidelines <- read.csv("data-raw/guidelines.csv", na.strings = c("NA", ""), stringsAsFactors = TRUE)

  n <- nrow(guidelines)
  guidelines <- merge(variables, guidelines, by = "Variable")
  stopifnot(n == nrow(guidelines))

  stopifnot(identical(colnames(guidelines),
                      c("Variable", "Code", "Jurisdiction",
                        "Use", "Samples", "Days", "Average",
                        "Condition", "Guideline", "Unit",
                        "Comments", "Date", "URL")))

  stopifnot(identical(levels(guidelines$Jurisdiction),
                      c("BC", "CA")))

  stopifnot(identical(levels(guidelines$Use),
                      c("Drinking", "Freshwater Life", "Irrigation", "Livestock",
                        "Marine Life", "Recreation", "Wildlife")))

  stopifnot(identical(levels(guidelines$Unit),
                      c("/dL", "m", "mg/L", "NTU", "pH", "ug/L")))

  guidelines$Comments <- as.character(guidelines$Comments)
  guidelines$Date <- as.Date(as.character(guidelines$Date))

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

  # check url if internet connection

  guidelines
}
rm(guidelines)
guidelines <- input_guidelines()
summary(guidelines)
devtools::use_data(guidelines, overwrite = TRUE, compress = "xz")

