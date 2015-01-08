check_columns_values <- function (x, by) {
  if(!"Code" %in% colnames(x))
    stop("x must contain a Code column. Codes can be generated from variable names using
        the get_codes() function.")

  if(!all(c("Value", "Units") %in% colnames(x)))
    stop("x must contain Value and Units columns")

  if(!is.null(by)) {

    if(!all(by %in% colnames(x)))
      stop("x must contain columns ", punctuate_strings(by, "and"), " in by")

    if("Date" %in% by) {
      message("Limits automatically calculated by Date")
      by <- by[!by %in% "Date"]
      if(length(by) == 0)
        by <- NULL
    }
  }

  if(!"Date" %in% colnames(x))
    warning("x missing a Date column. All values assume to have been taken on the same day.")
  by
}

get_limits_use <- function (use) {
  x <- dplyr::filter_(wqbc::limits, ~Use == use)
  x <- dplyr::select_(x, ~Code, ~LowerLimit, ~UpperLimit, ~Units, ~Samples, ~Period,
                      ~Condition, ~Variable, ~Use, ~Jurisdiction)
  x
}

join_values_limits <- function (x, y) {
  x <- dplyr::rename_(x, "ValueUnits" = "Units")
  x <- remove_columns_from_x_in_y(x, dplyr::select_(y, ~-Code))
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
  assert_that(is.null(by) || is.character(by))
  assert_that(is.string(use))

  if(!use %in% get_uses()) stop("use must be ", punctuate_strings(get_uses()))
  if(nrow(x) == 0) stop("x must contain at least one row of data")

  by <- check_columns_values(x, by)
  x <- join_values_limits(x, get_limits_use(use))
  #  x <- adjust_value_units(x)

  x <- tidy_up_values (x)
  x
}
