get_guidelines_use_jurisdiction <- function (use, jurisdiction) {
  guidelines <- dplyr::filter_(wqbc::guidelines, ~Use == use & Jurisdiction == jurisdiction)
  dplyr::select_(guidelines, ~Code, ~Guideline, ~Unit, ~Samples, ~Days,
                               ~Condition, ~Variable, ~Use, ~Jurisdiction)
}

#' Adds Water Quality Guidelines
#'
#' Adds water quality thresholds/objectives for British Columbia (BC) or
#' Canada (CA) for a range
#'
#' If the argument \code{x} is missing the function returns all
#' the guidelines for all the variables currently defined in the
#' wqbc package for the specified use and jurisdiction.
#'
#' @param x data.frame with column(s) Code or Variable
#' @param use string of required use
#' @param jurisdiction string of regulatory body
#' @examples
#' wq_add_guidelines()
#' wq_add_guidelines(use = "Drinking")
#' @export
wq_add_guidelines <- function (x, use = "Freshwater Life", jurisdiction = "BC") {
  assert_that(is.string(use))
  assert_that(is.string(jurisdiction))

  if(!use %in% wq_uses()) stop("use must be ", punctuate_strings(wq_uses()))

  if(!jurisdiction %in% wq_jurisdictions())
    stop("jurisdiction must be ", punctuate_strings(wq_jurisdictions()))

  glines <- get_guidelines_use_jurisdiction(use, jurisdiction)
  if(missing(x)) return (glines)

  assert_that(is.data.frame(x))
  if(nrow(x) == 0) stop("x must contain at least one row of data")

  if("Code" %in% colnames(x)) {
    x <- wq_add_variables(x)
  } else if("Variable" %in% colnames(x)) {
    x <- wq_add_codes(x)
  } else stop("x must contain the column(s) Code and/or Variable")

  x <- remove_columns_from_x_in_y(x, y = dplyr::select_(glines, ~-Code, ~-Variable))
  x <- dplyr::left_join(x, glines, by = c("Code", "Variable"))
  unknown <- is.na(x$Guideline)
  if(any(unknown)) {
    warning(sum(unknown), " values in x do not have a recognised guideline")
  }
  x
}
