check_rows <- function (x) {
  if(nrow(x) == 0)
    stop("x must contain at least one row of data")
  NULL
}

check_columns <- function (x, colnames) {
  stopifnot(is.data.frame(x))
  stopifnot(is.character(colnames))

  if("Code" %in% colnames) {
    if(!"Code" %in% colnames(x))
      stop("x must contain a Code column.
Codes can be generated from variable names using
         the get_codes() function.")
    colnames <- colnames[colnames != "Code"]
  }
  if(!all(colnames %in% colnames(x)))
    stop("x must contain columns ", punctuate_strings(by, "and"), ".")
  NULL
}

check_class_columns <- function (x, columns) {
  check_columns(x, names(columns))

  for(colname in names(columns)) {
    if(!class(x[[colname]]) %in% columns[[colname]])
      stop("column ", colname, " must be class ", punctuate_strings(columns[[colname]], "or"))
  }
  NULL
}

check_by <- function (by, x, res_names) {
  if(is.null(by))
    return (NULL)

  if(!all(by %in% colnames(x)))
    stop("x must contain columns ", punctuate_strings(by, "and"), " in by")

  if(any(by %in% res_names))
    stop("by must not include ", punctuate_strings(res_names, "and"))

  NULL
}

