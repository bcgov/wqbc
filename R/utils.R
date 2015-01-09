punctuate_strings <- function (x, qualifier = "or") {
  if(length(x) == 1)
    return (x)
  n <- length(x)
  paste(paste(x[-n], collapse = ", "), qualifier, x[n])
}

add_missing_columns <- function (x, columns, messages = TRUE) {
  assert_that(is.data.frame(x))
  assert_that(is.list(columns))
  assert_that(is.flag(messages) && noNA(messages))

  for(column in names(columns)) {
    if(!column %in% colnames(x)) {
      if(messages) message("adding missing column ", column, " to x")
      x[[column]] <-  columns[[column]]
    }
  }
  x
}

delete_columns <- function (x, colnames, messages = TRUE) {

  colnames <- colnames(x)[colnames(x) %in% colnames]
  if(length(colnames) >= 1) {
    if(messages)
      message("Removed columns ", punctuate_strings(colnames, "and"), " from x")
  }
  x <- x[, !colnames(x) %in% colnames, drop = FALSE]
  x
}

delete_rows_with_missing_values <- function (x, columns, messages = TRUE) {
  check_columns(x, unlist(columns))

  for(col in columns) {
    bol <- is.na(x[[col[1]]])

    if(length(col) > 1) {
      for(i in 2:length(col))
        bol <- bol & is.na(x[[col[i]]])
    }
    if(any(bol)) {
      if(messages) {
        message("filtered ", length(bol), " rows with missing values
                from column(s) ", punctuate_strings(col, "and"), " in x")
      }
      x <- x[!bol, , drop = FALSE]
    }
  }
  x
}

#' Substitute Units
#'
#' Where possible substitute units with
#' possible values
#'
#' @param x character vector of units to substitute
#' @param messages flag indicating whether to provide messages
#' @return character vector of substituted units where
#' match or NA
#' @examples
#' substitute_units(c("mg/L", "MG/L", "mg /L ", "Kg/l", "kg.l"), FALSE)
#' @export
substitute_units <-function (x, messages = TRUE) {
  assert_that(is.character(x) || is.factor(x))
  assert_that(is.flag(messages) && noNA(messages))

  x <- as.character(x)
  x <- tolower(x)
  x <- gsub(" ", "", x)
  units <- get_units()

  bol <- !is.na(x) & !x %in% tolower(units)
  if(any(bol)) {
    if(messages) {

      message("The following units are unrecognised and are replaced with a missing value: ",
           punctuate_strings(unique(x[bol]), "and"))
      message("To see possible units type get_units()")
    }
    is.na(x[bol]) <- TRUE
  }
  if(all(is.na(x)))
    return (x)

  x <- data.frame(x = x)
  units <- data.frame(x = tolower(units), units = units)

  x <- dplyr::left_join(x, units, by = "x")
  x <- as.character(x$units)
  x
}

#' Convert Units
#'
#' Converts units
#'
#' @param x numeric vector of values to convert
#' @param from character vector of original units
#' @param to character vector new units
#' @param messages flag indicating whether to print messages
#' @return numeric vector of values in new units
#' @examples
#' convert_units(1:10, from = "mg/L", to = "ug/L")
#' @export
convert_units <- function (x, from, to, messages = TRUE) {
  assert_that(is.numeric(x))
  assert_that(is.character(from) || is.factor(from))
  assert_that(is.character(to) || is.factor(to))
  assert_that(is.flag(messages) && noNA(messages))

  from <- substitute_units(from, messages)
  to <- substitute_units(to, messages)

  x <- x * get_unit_multiplier(from) / get_unit_multiplier(to)

  bol <- get_unit_type(from) != get_unit_type(to)

  if(any(bol, na.rm = TRUE)) {
    if(messages) {
      message(sum(bol, na.rm = TRUE), " values have inconvertible units")
    }
    is.na(x[!is.na(bol) & bol]) <- TRUE
  }
  x
}

## Assign and transform the coordinate system/projection to match the base BC map
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
  assert_that(is.numeric(x))
  assert_that(is.flag(na.rm))
  expm1(mean(log1p(x), na.rm = na.rm))
}
