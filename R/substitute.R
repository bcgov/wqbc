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

wqbc_substitute <- function (org, mod = org, sub, messages) {
  org <- as.character(org)
  mod <- as.character(mod)
  sub <- as.character(sub)

  orgd <- data.frame(org = org, match = tolower(mod), stringsAsFactors = FALSE)
  subd <- data.frame(sub = sub, match = tolower(sub), stringsAsFactors = FALSE)

  combd <- dplyr::left_join(orgd, subd, by = "match")

  if(messages) messages_match_substitution(combd$org, combd$sub)

  combd$sub
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
  x <- as.character(x)
  x <- gsub("ALUMINUM","ALUMINIUM", x)
  x <- gsub("Aluminum","Aluminium", x)
  x <- gsub("aluminum","aluminium", x)

  y <- unique(x)
  y <- as.list(y)
  names(y) <- y

  vars <- get_variables()
  if(!strict) {
    first_words <- stringr::word(vars)
    first_words <- unique(first_words[duplicated(first_words)])
    vars <- vars[!stringr::word(vars) %in% first_words]
  }
  vars <- as.list(vars)
  names(vars) <- vars

  y <- lapply(y, FUN = split_words_tolower)
  vars <- lapply(vars, FUN = split_words_tolower)

  y <- lapply(y, FUN = sub_vars, vars = vars, strict = strict)
  y <- unlist(y)

  if(length(y)) {
    if(messages) {
      yy <- y[y != names(y)]
      if(length(yy)) {
        message("Substituted ",
                punctuate_strings(paste(yy, "for", names(yy)), "and"), ".")
      }
    }
    bol <- x %in% names(y)
    x[bol] <- y[x[bol]]
  }
  x
}

