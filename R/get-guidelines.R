#' Get Water Quality Guidelines
#'
#' Gets water quality thresholds/objectives for British Columbia (BC) or
#' Canada (CA) for a range
#'
#' If the argument \code{x} is missing the function returns all
#' the guidelines for all the variables currently defined in the
#' wqbc package for the specified use and jurisdiction.
#'
#' @param x data.frame with column Variables or Code which
#' @param use string of required use
#' @param jurisdiction string of regulatory body
#' @examples
#' get_guidelines()
#' get_guidelines(use = "Drinking")
#' @export
get_guidelines <- function (x, use = "Freshwater Life", jurisdiction = "BC") {
  assert_that(is.string(use))
  assert_that(is.string(jurisdiction))

  if(!use %in% wq_uses())
    stop("use must be ", punctuate_strings(wq_uses()))

  if(!jurisdiction %in% wq_jurisdictions())
    stop("jurisdiction must be ", punctuate_strings(wq_jurisdictions()))

  guidelines <- filter_select_guidelines(use, jurisdiction)
  if(missing(x))
    return (guidelines)

  assert_that(is.data.frame(x))
  x <- remove_use_jurisdiction_columns(x, use, jurisdiction)



}
