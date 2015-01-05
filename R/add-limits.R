get_limits_use <- function (use) {
  limits <- dplyr::filter_(wqbc::limits, ~Use == use)
  dplyr::select_(limits, ~Code, ~LowerLimit, ~UpperLimit, ~Units, ~Samples, ~Days,
                               ~Condition, ~Variable, ~Use, ~Jurisdiction)
}

#' Adds Water Quality limits
#'
#' Adds water quality thresholds/objectives for British Columbia (BC) or
#' Canada (CA) for a range
#'
#' @param x data.frame with column(s) Code or Variable
#' @param use string of required use
#' @export
wq_add_limits <- function (x, use = "Freshwater Life") {
  assert_that(is.data.frame(x))
  assert_that(is.string(use))

  if(!use %in% wq_uses()) stop("use must be ", punctuate_strings(wq_uses()))

  glines <- get_limits_use(use)

  if(nrow(x) == 0) stop("x must contain at least one row of data")

  if("Code" %in% colnames(x)) {
    x <- wq_add_variables(x)
  } else if("Variable" %in% colnames(x)) {
    x <- wq_add_codes(x)
  } else stop("x must contain the column(s) Code and/or Variable")

  x <- remove_columns_from_x_in_y(x, y = dplyr::select_(glines, ~-Code, ~-Variable))
  x <- dplyr::left_join(x, glines, by = c("Code", "Variable"))
  unknown <- is.na(x$LowerLimit) | is.na(x$UpperLimit)
  if(any(unknown)) {
    warning(sum(unknown), " values in x do not have recognised limits")
  }
  x
}
