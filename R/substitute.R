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

#' Substitute Column Names
#'
#' Where possible substitute column names
#' with recognised column names
#'
#' @param x character vector of column names to substitute
#' @param messages flag indicating whether to print messages
#' @return character vector of substituted or original names
#' @examples
#' substitute_colnames(c("dates", "variables", "latitude", "longitude"))
#' @export
substitute_colnames <-function (x, messages = TRUE) {
  assert_that(is.character(x) || is.factor(x))
  assert_that(is.flag(messages) && noNA(messages))

  x <- as.character(x)
  x <- data.frame(x = tolower(x), original = x, stringsAsFactors = FALSE)
  x$x <- gsub(" ", "", x$x)
  x$x <- sub("^dates$", "date", x$x)
  x$x <- sub("^variables$", "variable", x$x)
  x$x <- sub("^values$", "value", x$x)
  x$x <- sub("^latitude$", "lat", x$x)
  x$x <- sub("^longitude$", "long", x$x)

  sub <- c("Date", "Variable", "Value", "Units", "Lat", "Long")

  substitute(x, sub, messages)
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

#' Substitute Names
#'
#' Where possible substitute  names
#' with recognised names
#'
#' @param x data.frame
#' @param messages flag indicating whether to print messages
#' @return data.frame
#' @examples
#'
#' x <- data.frame(variable = c("total phosphorus", "arsenic"),
#'                value = c(1, 2),
#'                units = c("MG / L", "Ug/l "))
#' substitute_names(x)
#'
#' @export
substitute_names <-function (x, messages = TRUE) {
  assert_that(is.data.frame(x))
  colnames(x) <- substitute_colnames(colnames(x), messages = messages)
  x$Units <- substitute_units(x$Units, messages = messages)
  x$Variable <- substitute_variables(x$Variable, messages = messages)
  x
}
