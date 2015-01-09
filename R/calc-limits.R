get_limits_use <- function (use) {
  x <- dplyr::filter_(wqbc::limits, ~Use == use)
  x <- dplyr::select_(x, ~Code, ~LowerLimit, ~UpperLimit, ~Units, ~Samples, ~Period,
                      ~Condition, ~Variable, ~Use, ~Jurisdiction)
  x
}

join_values_limits <- function (x, y) {
  x <- dplyr::rename_(x, "ValueUnits" = "Units")
  x <- delete_columns(x, colnames(dplyr::select_(y, ~-Code)))
  x <- dplyr::left_join(x, y, by = "Code")
  x
}

tidy_up_values <- function (x) {
  unknown <- is.na(x$LowerLimit) & is.na(x$UpperLimit)
  if(all(unknown))
    stop("no values in x with recognised limits")
  if(any(unknown)) {
    warning("deleting ", sum(unknown), " values in x with no limits")
    x <- x[!unknown,,drop = FALSE]
  }

  x$Variable <- droplevels(x$Variable)
  x$Variable <- droplevels(x$Variable)
  x$Period <- droplevels(x$Period)

  x$Units <- as.character(x$Units)
  x$ValueUnits <- as.character(x$ValueUnits)

  x$Use <- droplevels(x$Use)
  x$ValueUnits <- NULL
  x
}

#' Calculates Water Quality limits
#'
#' Calculates the approved lower and upper water quality thresholds for
#' the jurisdiction of British Columbia. If the Date column is not
#' supplied the data is assumed to have been collected on the same date.
#' Assumes values are individual readings.
#'
#' @param x data.frame with columns Code, Value and Units.
#' @param by character vector of columns to calculate limits by
#' @param use string of required use
#' @examples
#' data(fraser)
#' fraser <- calc_limits(fraser)
#' @export
calc_limits <- function (x, by = NULL, use = "Freshwater Life") {
  assert_that(is.data.frame(x))
  assert_that(is.null(by) || (is.character(by) && noNA(by)))
  assert_that(is.string(use))

  if(!use %in% get_uses()) stop("use must be ", punctuate_strings(get_uses()))

  check_rows(x)
  check_columns(x, c("Code", "Value", "Units"))
  x <- add_missing_columns(x, list("Date" = as.Date("2000-01-01")))
  check_class_columns(x, list("Code" = c("character", "factor"),
                              "Value" = "numeric",
                              "Units" = c("character", "factor"),
                              "Date" = "Date"))

  check_by(by, x, res_names = c("Code", "Value", "Units", "Date"))

  x <- join_values_limits(x, get_limits_use(use))
  #  x <- adjust_units_values(x)
  x <- tidy_up_values (x)
  x
}
