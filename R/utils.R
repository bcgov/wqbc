punctuate_strings <- function (x, qualifier = "or") {
  if(length(x) == 1)
    return (x)
  n <- length(x)
  paste(paste(x[-n], collapse = ", "), qualifier, x[n])
}

filter_select_guidelines <- function (use, jurisdiction) {
  guidelines <- dplyr::filter_(guidelines, ~Use == use & Jurisdiction == jurisdiction)
  guidelines <- dplyr::select_(guidelines, ~Code, ~Guideline, ~Unit, ~Samples, ~Days,
                               ~Condition, ~Variable, ~Use, ~Jurisdiction)

  guidelines
}

remove_use_jurisdiction_columns <- function (x, use, jurisdiction) {
  if("Use" %in% colnames(x)) {
    message("Use column in x replaced with ", use)
    x$Use <- NULL
  }
  if("Jurisdiction" %in% colnames(x)) {
    message("Jurisdiction column in x replaced with ", jurisdiction)
    x$Jurisdiction <- NULL
  }
  x
}

