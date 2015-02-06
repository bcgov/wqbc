check_rows <- function (x) {
  if(nrow(x) == 0) stop("x must contain at least one row of data")
  TRUE
}

check_columns <- function (x, colnames) {
  stopifnot(is.data.frame(x))
  stopifnot(is.character(colnames))

  colnames <- unique(colnames)

  bol <- colnames %in% colnames(x)
  if(!all(bol))
    stop("x must contain ", plural("column", sum(!bol) > 1, " "), punctuate_strings(colnames[!bol], "and"))
  TRUE
}

check_class_columns <- function (x, columns) {
  check_columns(x, names(columns))

  for(colname in names(columns)) {
    if(!class(x[[colname]]) %in% columns[[colname]])
      stop("column ", colname, " must be class ", punctuate_strings(columns[[colname]], "or"))
  }
  TRUE
}

check_by <- function (by, colnames, res_names) {
  if(is.null(by))
    return (TRUE)

  if(!all(by %in% colnames))
    stop("x must contain columns ", punctuate_strings(by, "and"), " in by")

  if(any(by %in% res_names))
    stop("by must not include ", punctuate_strings(res_names, "and"))
  TRUE
}
