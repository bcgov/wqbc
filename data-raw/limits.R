source("data-raw/codes.R")

check_limits <- function (x) {

  check_valid_expression <- function (x) {
    parse(text = x)
    TRUE
  }

  stopifnot(identical(colnames(x),
                      c("Variable", "Form", "Average",
                        "Condition", "LowerLimit", "UpperLimit", "Units", "Table")))

  stopifnot(all(!is.na(x$Variable)))
  stopifnot(all(!is.na(x$LowerLimit) | !is.na(x$UpperLimit)))
  stopifnot(all(!is.na(x$Units)))
  stopifnot(all(!is.na(x$Table)))

  stopifnot(all(x$Units %in% get_units()))

  check_valid_expression(x$Condition)
  check_valid_expression(x$LowerLimit)
  check_valid_expression(x$UpperLimit)

  NULL
}

input_limits <- function (codes) {
  limits <- read.csv("data-raw/limits.csv", na.strings = c("NA", ""), stringsAsFactors = FALSE)

  check_limits(limits)

  # lapply(limits, FUN = function (limits) (sort(unique(limits))))
  #   write.csv(limits, "data-raw/limits.csv", row.names = FALSE)

  limits <- rename_(limits, "..Units" = "Units")

  n <- nrow(limits)
  limits <- left_join(limits, codes, by = "Variable")
  stopifnot(n == nrow(limits))

  stopifnot(all(limits$..Units == limits$Units))
  limits$..Unit <- NULL

  limits %<>% dplyr::filter(
    !is.na(Variable) &
      !is.na(Code) &
      !(is.na(LowerLimit) & is.na(UpperLimit)) &
      !is.na(Units))

  limits %<>% arrange(Variable, Average)

  limits %<>% select(Variable, Code,
                     Average, Condition, LowerLimit, UpperLimit, Units, Form)

  limits$Code %<>% factor
  limits$Average %<>% factor
  limits$Variable %<>% factor
  limits$Form %<>% factor
  limits$Units %<>% droplevels

  limits
}
limits <- input_limits(codes)
devtools::use_data(limits, overwrite = TRUE, compress = "xz")
