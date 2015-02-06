messages_match_substitution <- function (x, y, txt = "substitute") {
  x <- as.character(x)
  y <- as.character(y)
  d <- data.frame(x = x, y = y, stringsAsFactors = FALSE)
  d <- unique(d)
  d <- dplyr::arrange_(d, ~x)
  b <- !is.na(d$x) & !is.na(d$y) & d$x != d$y
  if(any(b)) {
    db <- d[b,,drop = FALSE]
    message(capitalize(txt), "d ",
            punctuate_strings(paste0("'", db$x, "' with '", db$y, "'"), "and"), ".")
  }
  b <- !is.na(d$x) & is.na(d$y)
  if(any(b)) {
    db <- d[b,,drop = FALSE]
    message("Failed to ", txt, " ",
            punctuate_strings(paste0("'", db$x, "'"), "and"), ".")
  }
  NULL
}

all_words_in_x_in_y_one <- function (x) {
  all(strsplit(x[1], " ")[[1]] %in% strsplit(x[2], " ")[[1]])
}

all_words_in_x_in_y <- function (x, y) {
  mat <- as.matrix(data.frame(x = x, y = y))
  apply(mat, MARGIN = 1, all_words_in_x_in_y_one)
}

wqbc_substitute <- function (org, mod = org, sub, sub_mod = sub, messages) {
  org <- as.character(org)
  mod <- as.character(mod)
  sub <- as.character(sub)
  sub_mod <- as.character(sub_mod)

  orgd <- data.frame(org = org, match = tolower(mod), stringsAsFactors = FALSE)
  subd <- data.frame(sub = sub, match = tolower(sub_mod), stringsAsFactors = FALSE)

  orgd$sub <- NA_character_
  for(i in 1:nrow(subd)) {
    bol <- all_words_in_x_in_y(subd$match[i], orgd$match)
    if(any(bol)) {
      stopifnot(all(is.na(orgd$sub[bol])))
      orgd$sub[bol] <- subd$sub[i]
    }
  }
  if(messages) messages_match_substitution(orgd$org, orgd$sub)
  orgd$sub
}

#' Substitute Units
#'
#' Where possible substitute units with
#' recognised values. Returns a character vector of the substituted or original units.
#'
#' @param x The character vector of units to substitute.
#' @param messages A flag indicating whether to print messages.
#' @examples
#' substitute_units(c("mg/L", "MG/L", "mg /L ", "Kg/l", "gkl", "CFU/100ML"))
#' substitute_units(c("MG/L", "MG/L", "MG/L"))
#' substitute_units("gkl")
#' substitute_units(c(NA, "mg/L"))
#' @export
substitute_units <- function (
  x, messages = getOption("wqbc.messages", default = TRUE)) {
  assert_that(is.character(x) || is.factor(x))
  assert_that(is.flag(messages) && noNA(messages))

  x <- as.character(x)

  y <- gsub("units", "", x, ignore.case = TRUE)
  y <- gsub(" ", "", y)
  y <- gsub("100mL", "dL", y, ignore.case = TRUE)

  wqbc_substitute(org = x, mod = y, sub = lookup_units(), messages = messages)
}

#' Substitute Variable Names
#'
#' Where possible substitute variable names
#' with recognised variable names
#'
#' @param x The character vector of variable names to substitute.
#' @param strict A flag indicating whether to require all words
#' in a variable name to be present in or only
#' the first word one. Note when strict = FALSE
#' ambiguous variables such as "Iron Dissolved"
#' and "Iron Total" are dropped.
#' @param messages A flag indicating whether to print messages.
#' @examples
#' substitute_variables(c("ALUMINIUM SOMETHING", "FLUORIDE DISSOLVED",
#' "FLUORIDE", "NITROGEN DISSOLVED NITRATE", "PHOSPHORUS - TOTAL",
#' "KRYPTONITE", "OXYGEN", "OXYGEN DISSOLVED"))
#' @export
substitute_variables <-function (
  x, strict = TRUE, messages = getOption("wqbc.messages", default = TRUE)) {

  assert_that(is.character(x) || is.factor(x))
  assert_that(is.flag(strict) && noNA(strict))
  assert_that(is.flag(messages) && noNA(messages))

  x <- as.character(x)

  y <- gsub("Aluminum", "Aluminium", x, ignore.case = TRUE)

  sub <- lookup_variables()
  sub_mod <- sub
  if(!strict) { # pull out first word and remove duplicates
    sub_mod <- stringr::word(sub)
    bol <- sub_mod %in% unique(sub_mod[duplicated(sub_mod)])
    sub <- sub[!bol]
    sub_mod <- sub_mod[!bol]
  }
  wqbc_substitute(org = x, mod = y, sub = sub, sub_mod = sub_mod, messages = messages)
}
