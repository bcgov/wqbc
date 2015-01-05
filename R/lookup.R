#' Get Water Quality Uses
#'
#' Returns a character vector of the uses for which
#' limits are currently defined in the wqbc package.
#' @examples
#' wq_uses()
#'
#' @export
wq_uses <- function () {
  levels(wqbc::limits$Use)
}

#' Get Water Quality Variables
#'
#' Returns a character vector of the water quality variables for which
#' limits are currently defined in the wqbc package.
#' @examples
#' wq_variables()
#'
#' @export
wq_variables <- function () {
  levels(wqbc::limits$Variable)
}

#' Get Water Quality Codes
#'
#' Returns a character vector of the water quality codes for which
#' limits are currently defined in the wqbc package.
#'@examples
#' wq_codes()
#'
#' @export
wq_codes<- function () {
  levels(wqbc::limits$Code)
}

#' Get Water Quality Code-Variable Lookup
#'
#' Returns a data.frame of the water quality code and variable
#' look up table currently defined in the wqbc package.
#' @examples
#' wq_codes_variables()
#'
#' @export
wq_codes_variables <- function () {
  x <- dplyr::select_(wqbc::limits, ~Code, ~Variable)
  x <- unique(x)
  dplyr::arrange_(x, ~Code)
}

#' Adds Code Column
#'
#' Adds a Code column to the data.frame x based on the Variable column.
#' Unknown variable names are dropped.
#'
#' @param x data.frame with Variable column
#' @return data.frame with Variable and Code columns
#'
#' @export
wq_add_codes <- function (x) {
  assert_that(is.data.frame(x))

  if("Variable" %in% colnames(x))
    stop("x must contain column Variable")

  lookup <- wq_codes_variables()
  unknown <- !x$Variable %in% lookup$Variable

  if(all(unknown)) {
    stop("None of the variable names in x are recognised.
         To see the possible values type wq_codes_variables().")
  } else if(any(unknown)) {
    warning("Removed the unrecognised variable names ", punctuate_strings(sort(unique(x$Variable[unknown])), "and"), "from x")
    message("To see the possible values type wq_codes_variables()")
  }
  if("Code" %in% colnames(x)) {
    warning("Replaced Code column in x")
    x$Code <- NULL
  }
  dplyr::inner_join(dplyr::select_(lookup, ~Code, ~Variable), x, by = "Variable")
}

#' Adds Variable Column
#'
#' Adds a Variable column to the data.frame x based on the Code column.
#' Unknown variable names are dropped.
#'
#' @param x data.frame with Code column
#' @return data.frame with Variable and Code columns
#'
#' @export
wq_add_variables <- function (x) {
  assert_that(is.data.frame(x))

  if("Code" %in% colnames(x))
    stop("x must contain column Code")

  lookup <- wq_codes_variables()
  unknown <- !x$Code %in% lookup$Code

  if(all(unknown)) {
    stop("None of the code names in x are recognised.
         To see the possible values type wq_codes_variables().")
  } else if(any(unknown)) {
    warning("Removed the unrecognised code names ", punctuate_strings(sort(unique(x$Variable[unknown])), "and"), "from x")
    message("To see the possible values type wq_codes_variables()")
  }
  if("Variable" %in% colnames(x)) {
    warning("Replaced Variable column in x")
    x$Variable <- NULL
  }
  dplyr::inner_join(dplyr::select_(lookup, ~Code, ~Variable), x, by = "Code")
}
