source("R/codes.R")

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

check_limits <- function (x) {

  check_valid_expression <- function (x) {
    parse(text = x)
    TRUE
  }

  stopifnot(all(!is.na(x$Variable)))
  stopifnot(all(!is.na(x$Jurisdiction)))
  stopifnot(all(!is.na(x$Use)))
  stopifnot(all(!is.na(x$Samples)))
  stopifnot(all(!is.na(x$Days)))
  stopifnot(all(!is.na(x$Average)))
  stopifnot(all(!is.na(x$LowerLimit) | !is.na(x$UpperLimit)))
  stopifnot(all(!is.na(x$Units)))

  stopifnot(identical(colnames(x),
                      c("Variable", "Form", "Jurisdiction",
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

  stopifnot(all(x$Units %in% get_units()))

  stopifnot(identical(sort(unique(x$Status)),
                      c("Approved")))

  stopifnot(is.integer(x$Samples))
  stopifnot(is.integer(x$Days))

  check_valid_expression(x$Condition)
  check_valid_expression(x$LowerLimit)
  check_valid_expression(x$UpperLimit)

  NULL
}

input_limits <- function (codes) {
  limits <- read.csv("data-raw/limits.csv", na.strings = c("NA", ""), stringsAsFactors = FALSE)

  check_limits(limits)


  # lapply(limits, FUN = function (limits) (sort(unique(limits))))
  # write.csv(limits, "data-raw/limits.csv", row.names = FALSE)

  limits %<>% reassign_aquatic_life()
  limits %<>% set_periods()

  limits$Status %<>% factor(levels = c("Approved"))
  limits$Jurisdiction %<>% factor(levels = c("BC", "CA"))
  limits$Average %<>% factor(levels = c("geomean1", "max", "mean", "median"))
  limits$Use %<>% factor(levels = c(
    "Freshwater Life", "Marine Life", "Drinking", "Livestock",
    "Wildlife", "Irrigation", "Recreation", "Industrial"))

  limits <- rename_(limits, "..Units" = "Units")
  limits <- inner_join(codes, limits, by = "Variable")

  stopifnot(all(limits$..Unit == limits$Unit))
  limits$..Unit <- NULL

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

  limits %<>% arrange(Code, Form, Use, SubUse, Jurisdiction, Samples, Period)

  limits %<>% select(Code, Variable, Form, Jurisdiction, Use, SubUse,
                     Samples, Period,
                     Average, Condition, LowerLimit, UpperLimit, Units)

  limits$Code %<>% factor
  limits$Variable %<>% factor
  limits$Form %<>% factor
  limits$Jurisdiction %<>% droplevels
  limits$Average %<>% droplevels
  limits$Units %<>% droplevels

  limits
}
limits <- input_limits(codes)
summary(limits)
devtools::use_data(limits, overwrite = TRUE, compress = "xz")
