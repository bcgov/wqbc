use_guidelines <- function () {
  require(devtools)

  guidelines <- read.csv("data-raw/guidelines.csv", na.strings = c("NA", ""), stringsAsFactors = TRUE)

  stopifnot(identical(colnames(guidelines),
                      c("Parameter", "Form", "Jurisdiction",
                        "Use", "Samples", "Days", "Average",
                        "Condition", "Guideline", "Unit",
                        "Comments", "Date", "URL")))

  stopifnot(identical(levels(guidelines$Form),
                      c("Dissolved", "E.coli", "Enterococci", "Total")))

  stopifnot(identical(levels(guidelines$Jurisdiction),
                      c("British Columbia", "Canada")))

  stopifnot(identical(levels(guidelines$Use),
                      c("Drinking", "Freshwater Life", "Irrigation", "Livestock",
                        "Marine Life", "Recreation", "Wildlife")))

  stopifnot(identical(levels(guidelines$Unit),
                      c("/100mL", "C", "m", "mg/L", "NTU", "ug/L")))

  stopifnot(is.integer(guidelines$Samples))
  stopifnot(is.integer(guidelines$Days))

  stopifnot(all(!is.na(guidelines$Parameter)))
  stopifnot(all(!is.na(guidelines$Jurisdiction)))
  stopifnot(all(!is.na(guidelines$Use)))
  stopifnot(all(!is.na(guidelines$Samples)))
  stopifnot(all(!is.na(guidelines$Days)))
  stopifnot(all(!is.na(guidelines$Average)))
#  stopifnot(all(!is.na(guidelines$Guideline)))
#  stopifnot(all(!is.na(guidelines$Unit)))
  stopifnot(all(!is.na(guidelines$Date)))
#  stopifnot(all(!is.na(guidelines$Url)))

  guidelines$Comments <- as.character(guidelines$Comments)
  guidelines$Date <- as.Date(as.character(guidelines$Date))

# check url if internet connection

  devtools::use_data(guidelines, overwrite = TRUE, compress = "xz")

  invisible(guidelines)
}

summary(use_guidelines())
