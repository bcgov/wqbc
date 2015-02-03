wqbc_codes <- function () {
  codes <- codes
  codes$Code <- as.character(codes$Code)
  codes$Variable <- as.character(codes$Variable)
  codes$Units <- as.character(codes$Units)
  codes
}

wqbc_limits <- function () {
  limits <- limits
  limits$Variable <- as.character(limits$Variable)
  limits$Units <- as.character(limits$Units)
  limits
}

#' Get Units
#'
#' Gets recognised units.
#'
#' @return A character vector of the recognised units.
#' @examples
#' get_units()
#' @export
get_units <- function () {
  c("ng/L", "ug/L", "mg/L", "g/L", "kg/L", "pH")
}

get_unit_multiplier <- function (x) {
  units <- c("ng/L" = 10^-9, "ug/L" = 10^-6, "mg/L" = 10^-3,
             "g/L" = 1,  "kg/L" = 10^3,
             "pH" = 1)
  x <- units[x]
  names(x) <- NULL
  x
}

get_unit_type <- function (x) {
  type <- list("concentration" = c("ng/L", "ug/L", "mg/L", "g/L", "kg/L"),
               "pH" = "pH")

  type <- unlist(type)
  names <- sub("\\d$", "", names(type))
  values <- type
  type <- names
  names(type) <- values

  x <- type[x]
  names(x) <- NULL
  x
}

convert_values <- function (x, from, to, messages) {

  from <- substitute_units(from, messages = messages)
  to <- substitute_units(to, messages = messages)

  x <- x * get_unit_multiplier(from) / get_unit_multiplier(to)

  bol <- from != to & get_unit_type(from) != get_unit_type(to)
  bol <- is.na(bol) | bol

  if(any(bol)) {
    warning(sum(bol), " values have inconvertible units")
    is.na(x[bol]) <- TRUE
  }
  x
}

#' Get Variables
#'
#' Gets recognised water quality variables.
#'
#' @param codes An optional character vector of the codes to get the variables for.
#' @param messages A flag indicating whether to print messages.
#' @return A character vector of the water quality variables.
#' @examples
#' get_variables()
#' get_variables(get_codes())
#' get_variables(c(get_codes()[1], "EMS_WTF_"))
#' @export
get_variables<- function (
  codes = NULL, messages = getOption("wqbc.messages", default = TRUE)) {
  if(is.null(codes)) return (wqbc_codes()$Variable)

  assert_that(is.character(codes) || is.factor(codes))
  codes <- as.character(codes)

  y <- gsub("[-]", "_", codes)
  bol <- !is.na(y) & substr(y,1,4) != "EMS_"
  y[bol] <- paste0("EMS_", y[bol])

  d <- dplyr::left_join(data.frame(Code = y, stringsAsFactors = FALSE),
                        wqbc_codes(), by = "Code")

  if(messages) messages_match_substitution(codes, d$Variable, "replace")

  as.character(d$Variable)
}

#' Get Codes
#'
#' Gets the recognised water quality codes.
#'
#' @param variables An optional character vector of variables to get codes for.
#' @param add_na A flag indicating whether to replace variables without codes
#' with missing values.
#'@examples
#' get_codes()
#' @export
get_codes <- function (variables = NULL, add_na = TRUE) {
  assert_that(is.null(variables) || is.character(variables) || is.factor(variables))
  assert_that(is.flag(add_na) && noNA(add_na))

  if(is.null(variables)) return (wqbc_codes()$Code)

  variables <- as.character(variables)
  x <- dplyr::left_join(data.frame(Variable = variables, stringsAsFactors = FALSE),
                        wqbc_codes(), by = "Variable")
  x$Code <- as.character(x$Code)
  x$Variable <- as.character(x$Variable)
  if(!add_na) {
    bol <- is.na(x$Code)
    x$Code[bol] <- x$Variable[bol]
  }
  x$Code
}

substitute_messages <- function (x, bol) {
  bol <- bol & x$sub != x$original
  if(any(bol)) {
    x <- unique(x[bol,,drop = FALSE])
    x <- dplyr::arrange_(x, ~sub)
    message("Substituted ", punctuate_strings(paste(x$sub, "for", x$original), "and"), ".")
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

wqbc_substitute2 <- function (org, mod = org, sub, messages) {
  org <- as.character(org)
  mod <- as.character(mod)
  sub <- as.character(sub)

  orgd <- data.frame(org = org, match = tolower(mod), stringsAsFactors = FALSE)
  subd <- data.frame(sub = sub, match = tolower(sub), stringsAsFactors = FALSE)

  combd <- dplyr::left_join(orgd, subd, by = "match")

  if(messages) messages_match_substitution(combd$org, combd$sub)

  combd$sub
}

#' Substitute Units
#'
#' Where possible substitute units with
#' recognised values.
#'
#' @param x The character vector of units to substitute.
#' @param messages A flag indicating whether to print messages.
#' @return A character vector of the substituted or original units.
#' @examples
#' substitute_units(c("mg/L", "MG/L", "mg /L ", "Kg/l", "gkl", "CFU/100ML"))
#' substitute_units(c("MG/L", "MG/L", "MG/L"))
#' substitute_units("gkl")
#' substitute_units(c(NA, "mg/L"))
#' @export
substitute_units <- function (
  x, messages = getOption("wqbc.messages", default = TRUE)) {
  assert_that(is.character(x) || is.factor(x))

  x <- as.character(x)

  y <- gsub("units", "", x, ignore.case = TRUE)
  y <- gsub(" ", "", y)
  y <- gsub("100mL", "dL", y, ignore.case = TRUE)

  wqbc_substitute2(x, y, sub = get_units(), messages)
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
#' @return A character vector of the substituted or original names.
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

standardize_wqdata_variable <- function (x, messages) {
  codes <- wqbc_codes()
  codes <- dplyr::filter_(codes, ~Variable == x$Variable[1])
  x$Value <- convert_values(x$Value, from = x$Units, to = codes$Unit,
                            messages = messages)
  x$Units <- codes$Unit
  x
}

#' Standardize Water Quality Data
#'
#' Standardizes a water quality data set so that only recognised
#' variables and units remain and values have consistent units.
#' Negative or missing values are removed.
#'
#' @details Note when strict = FALSE
#' ambiguous variables such as "Iron Dissolved"
#' and "Iron Total" are dropped.
#'
#' @param x The data.frame to standardize.
#' @param strict A flag indicating whether to require all words
#' in a variable name to be present or only the first word.
#' @param messages A flag indicating whether to print messages.
#' @examples
#' standardize_wqdata(wqbc::dummy, messages = TRUE)
#' @export
standardize_wqdata <- function (
  x, strict = TRUE, messages = getOption("wqbc.messages", default = TRUE)) {
  assert_that(is.data.frame(x))
  assert_that(is.flag(strict) && noNA(strict))
  assert_that(is.flag(messages) && noNA(messages))

  if(messages) message("Standardizing water quality data...")

  check_columns(x, c("Variable", "Value", "Units"))

  if(is.factor(x$Variable)) x$Variable <- as.character(x$Variable)
  if(is.factor(x$Units)) x$Units <- as.character(x$Units)

  check_class_columns(x, list("Variable" = "character",
                              "Value" = "numeric",
                              "Units" = "character"))

  if(!nrow(x)) { message("Standardized."); return (x) }

  x$Variable <- substitute_variables(x$Variable, strict = strict, messages = messages)
  x$Units <- substitute_units(x$Units, messages = messages)

  is.na(x$Variable[!x$Variable %in% get_variables()]) <- TRUE
  is.na(x$Units[!x$Units %in% get_units()]) <- TRUE

  x$Value <- replace_negative_values_with_na(x$Value, messages = messages)

  x <- delete_rows_with_missing_values(x, columns = c("Variable", "Value", "Units"),
                                       messages = messages)

  if(!nrow(x)) { message("Standardized."); return (x) }

  x <- plyr::ddply(x, .variables = "Variable",
                   .fun = standardize_wqdata_variable, messages = messages)

  message("Standardized.")
  x
}
