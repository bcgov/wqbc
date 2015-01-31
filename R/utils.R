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
      x[[column]] <-  columns[[column]]
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

delete_rows_with_missing_values <- function (x, columns, messages) {
  if(missing(columns))
    columns <- as.list(colnames(x))

  check_columns(x, unlist(columns))

  for(col in columns) {
    bol <- is.na(x[[col[1]]])

    if(length(col) > 1) {
      for(i in 2:length(col))
        bol <- bol & is.na(x[[col[i]]])
    }
    if(any(bol)) {
      if(messages) {
        message("Deleted ", sum(bol),
                plural(" row", sum(bol) > 1), " with unrecognised values in ",
                punctuate_strings(col, "or"), ".")
      }
      x <- x[!bol, , drop = FALSE]
    }
  }
  x
}

replace_negative_values_with_na <- function (x, zero = FALSE, messages) {
  if(!zero) {
    bol <- !is.na(x) & x < 0
  } else {
    bol <- !is.na(x) & x <= 0
  }
  if(any(bol)) {
    if(messages) message("Replaced ", sum(bol), " negative ", ifelse(zero, "or zero ", ""),
                         plural("value", sum(bol) > 1), " with a missing value.")
    is.na(x[bol]) <- TRUE
  }
  x
}

proj_bc <- function (data, x, y, input_proj = NULL) {

  if(!requireNamespace("sp", quietly = TRUE))
    stop("sp package not installed")

  if(!requireNamespace("rgdal", quietly = TRUE))
    stop("rgdal package not installed")

  if (is.null(input_proj)) {
    input_proj <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
  }
  output_proj <- "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

  sp::coordinates(data) <- c(x,y)
  sp::proj4string(data) <- sp::CRS(input_proj)
  data <- sp::spTransform(data, sp::CRS(output_proj))
  as.data.frame(data)
}

is.error <- function (x) inherits (x, "try-error")
is.Date <- function (x) inherits (x, "Date")

#' Geometric Mean Plus-Minus 1
#'
#' Calculates geometric mean by adding 1 before logging
#' and subtracting 1 before exponentiating so that
#' geometric mean of
#' @param x numeric vector of non-negative numbers
#' @param na.rm flag indicating whether to remove missing values
#' @return number
#' @examples
#' mean(0:9)
#' geomean1(0:9)
#' @export
geomean1 <- function (x, na.rm = FALSE) {
  assert_that(is.vector(x))
  assert_that(is.flag(na.rm) && noNA(na.rm))
  x <- as.numeric(x)

  if(any(x < 0, na.rm = TRUE))
    stop("x must not be negative")

  expm1(mean(log1p(as.numeric(x)), na.rm = na.rm))
}

