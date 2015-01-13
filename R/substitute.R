substitute_messages <- function (x, bol) {
  if(any(bol)) {
    x <- unique(x[bol,,drop = FALSE])
    x <- dplyr::arrange_(x, ~sub)
    message("substituting ", punctuate_strings(paste(x$sub, "for", x$original), "and"))
  }
}

substitute <- function (x, sub, messages) {
  sub <- data.frame(x = tolower(sub), sub = sub, stringsAsFactors = FALSE)
  x <- dplyr::left_join(x, sub, by = "x")
  bol <- !is.na(x$sub) & x$original != x$sub
  if(messages) substitute_messages(x, bol)
  x$original[bol] <- x$sub[bol]
  x$original
}

#' Substitute Units
#'
#' Where possible substitute units with
#' recognised values
#'
#' @param x character vector of units to substitute
#' @param messages flag indicating whether to print messages
#' @return character vector of substituted or original units
#' @examples
#' substitute_units(c("mg/L", "MG/L", "mg /L ", "Kg/l", "gkl"))
#' substitute_units(c("MG/L", "MG/L", "MG/L"))
#' substitute_units("gkl")
#' substitute_units(c(NA, "mg/L"))
#' @export
substitute_units <-function (x, messages = TRUE) {
  assert_that(is.character(x) || is.factor(x))
  assert_that(is.flag(messages) && noNA(messages))

  x <- as.character(x)
  x <- data.frame(x = tolower(x), original = x, stringsAsFactors = FALSE)
  x$x <- gsub(" ", "", x$x)

  substitute(x, sub = get_units(), messages)
}

#' Substitute Variable Names
#'
#' Where possible substitute variable names
#' with recognised variable names
#'
#' @param x character vector of variable names to substitute
#' @param messages flag indicating whether to print messages
#' @return character vector of substituted or original names
#' @examples
#' substitute_variables(c("aluminum", "totalNitrogen"))
#' @export
substitute_variables <-function (x, messages = TRUE) {
  assert_that(is.character(x) || is.factor(x))
  assert_that(is.flag(messages) && noNA(messages))

  x <- as.character(x)
  x <- data.frame(x = tolower(x), original = x, stringsAsFactors = FALSE)
  x$x <- gsub(" ", "", x$x)

  substitute(x, sub = get_variables(), messages)
}
