plural <- function (x, s = FALSE, end = "") {
  paste0(x, ifelse(s, "s", ""), end)
}

punctuate_strings <- function (x, qualifier = "or") {
  if(length(x) == 1)
    return (x)
  n <- length(x)
  paste(paste(x[-n], collapse = ", "), qualifier, x[n])
}

add_missing_columns <- function (x, columns, messages) {
  assert_that(is.data.frame(x))
  assert_that(is.list(columns))
  assert_that(is.flag(messages) && noNA(messages))

  for(column in names(columns)) {
    if(!column %in% colnames(x)) {
      if(messages) message("Added missing column ", column, " to x.")
      if(nrow(x)) {
        x[[column]] <-  columns[[column]]
      } else {
        x[[column]] <-  columns[[column]][-1]
      }
    }
  }
  x
}

delete_columns <- function (x, colnames, messages) {
  colnames <- colnames(x)[colnames(x) %in% colnames]
  if(length(colnames) >= 1) {
    if(messages)
      message("Deleting ", plural("column", length(colnames) > 1, " "),
              punctuate_strings(colnames, "and"), " from x.")
  }
  x <- x[, !colnames(x) %in% colnames, drop = FALSE]
  x
}

del_cols_not_in_y <- function (x, y) {
  delete_columns(x, colnames(x)[!colnames(x) %in% y], messages = FALSE)
}

delete_rows_with_certain_values <- function (x, columns, messages, txt = "missing") {
  if(missing(columns))
    columns <- as.list(colnames(x))

  check_columns(x, unlist(columns))

  if(txt %in% c("missing", "unrecognised")) {
    fun <- function (x) is.na(x)
  } else if(txt == "negative") {
    fun <- function (x) !is.na(x) & x < 0
  }  else if(txt == "zero") {
    fun <- function (x) !is.na(x) & x == 0
  } else if (txt %in% c("missing or negative", "negative or missing")) {
    fun <- function (x) is.na(x) | x < 0
  } else stop()

  for(col in columns) {
    bol <- fun(x[[col[1]]])

    if(length(col) > 1) {
      for(i in 2:length(col))
        bol <- bol & fun(x[[col[i]]])
    }
    if(any(bol)) {
      if(messages) {
        message("Deleted ", sum(bol),
                plural(" row", sum(bol) > 1), " with ", txt, " values in ",
                punctuate_strings(col, "and"), ".")
      }
      x <- x[!bol, , drop = FALSE]
    }
  }
  x
}

capitalize <- function (x) {
    gsub(pattern = "\\b([a-z])", replacement = "\\U\\1", x, perl = TRUE)
}

is_match_words <- function (var, x, strict) {
  if(!strict) return (var[1] %in% x)
  all(var %in% x)
}

sub_vars <- function (x, vars, strict) {
  names(which(sapply(vars, FUN = is_match_words, x = x, strict = strict)))
}

split_words_tolower <- function (x) {
  tolower(unlist(strsplit(unlist(x), " ")))
}
