use_guidelines <- function () {
  require(devtools)

  guidelines <- read.csv("data-raw/guidelines.csv", na.strings = c("NA", ""), stringsAsFactors = TRUE)

  stopifnot(identical(colnames(guidelines),
                      c("Parameter", "Form", "Jurisdiction",
                        "Use", "Samples", "Days", "Function",
                        "Condition", "Guideline", "Unit",
                        "Date", "URL", "Comments")))

  stopifnot(identical(levels(guidelines$Form),
                      c("Dissolved", "E.coli", "Enterococci", "Total")))

  stopifnot(identical(levels(guidelines$Jurisdiction),
                      c("British Columbia", "Canada")))

  stopifnot(identical(levels(guidelines$Use),
                      c("Drinking", "Freshwater Life", "Irrigation", "Livestock",
                        "Marine Life", "Recreation", "Wildlife")))

  stopifnot(identical(levels(guidelines$Unit),
                      c("/100mL", "mg/L", "ug/L")))

  stopifnot(is.integer(guidelines$Samples))
  stopifnot(is.integer(guidelines$Days))

  guidelines$Comments <- as.character(guidelines$Comments)
  guidelines$Date <- as.Date(as.character(guidelines$Date))

  devtools::use_data(guidelines, overwrite = TRUE, compress = "xz")

  invisible(guidelines)
}

use_guidelines()
