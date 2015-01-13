substitute_messages <- function (x, bol) {
  if(any(bol)) {
    x <- unique(x[bol,,drop = FALSE])
    x <- dplyr::arrange_(x, ~sub)
    message("substituting ", punctuate_strings(paste(x$sub, "for", x$original), "and"))
  }
}

wqbc_substitute <- function (x, sub, messages) {
  sub <- data.frame(x = tolower(sub), sub = sub, stringsAsFactors = FALSE)
  x$sub <- NULL
  x$x <- tolower(x$x)
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
  x$x <- gsub("units", "", x$x, ignore.case = TRUE)
  x$x <- gsub("CFU", "", x$x, ignore.case = TRUE)
  x$x <- gsub(" ", "", x$x)
  x$x <- gsub("100mL", "dL", x$x)


  wqbc_substitute(x, sub = get_units(), messages)
}

is_match_words <- function (var, x) {
  all(var %in% x)
}

sub_vars <- function (x, vars) {
  names(which(sapply(vars, FUN = is_match_words, x = x)))
}

split_words_tolower <- function (x) {
  tolower(unlist(strsplit(unlist(x), " ")))
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
#' substitute_variables(c("ALUMINIUM SOMETHING", "FLUORIDE DISSOLVED",
#' "FLUORIDE", "NITROGEN DISSOLVED NITRATE", "PHOSPHORUS - TOTAL",
#' "KRYPTONITE", "OXYGEN", "OXYGEN DISSOLVED"))
#' @export
substitute_variables <-function (x, messages = TRUE) {
  assert_that(is.character(x) || is.factor(x))
  assert_that(is.flag(messages) && noNA(messages))

  x <- as.character(x)

  y <- unique(x)
  y <- as.list(y)
  names(y) <- y

  vars <- as.character(wqbc::codes$Variable)
  vars <- as.list(vars)
  names(vars) <- vars

  y <- lapply(y, FUN = split_words_tolower)
  vars <- lapply(vars, FUN = split_words_tolower)

  y <- lapply(y, FUN = sub_vars, vars = vars)
  y <- unlist(y)

  if(length(y)) {
    if(messages) {
      message("substituting ", punctuate_strings(paste(y, "for", names(y)), "and"))
    }
    bol <- x %in% names(y)
    x[bol] <- y[x[bol]]
  }
  x
}
