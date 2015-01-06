get_limits_use <- function (use) {
  limits <- dplyr::filter_(wqbc::limits, ~Use == use)
  dplyr::select_(limits, ~Code, ~LowerLimit, ~UpperLimit, ~Units, ~Samples, ~Days,
                               ~Condition, ~Variable, ~Use, ~Jurisdiction)
}

#' Gets Water Quality limits
#'
#' Gets lower and upper water quality thresholds for British Columbia
#'
#' @param x data.frame with columns Code and Value
#' @param use string of required use
#' @export
calc_limits <- function (x, use = "Freshwater Life") {
  assert_that(is.data.frame(x))
  assert_that(is.string(use))

  if(!use %in% get_uses()) stop("use must be ", punctuate_strings(get_uses()))

  glines <- get_limits_use(use)

  if(nrow(x) == 0) stop("x must contain at least one row of data")

  x <- remove_columns_from_x_in_y(x, y = dplyr::select_(glines, ~-Code, ~-Variable))
  x <- dplyr::left_join(x, glines, by = c("Code", "Variable"))
  unknown <- is.na(x$LowerLimit) | is.na(x$UpperLimit)
  if(any(unknown)) {
    warning(sum(unknown), " values in x do not have recognised limits")
  }
  x
}
