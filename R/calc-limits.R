join_codes <- function (x) {
  x <- dplyr::rename_(x, "..Units" = "Units")
  x$Variable <- as.character(x$Variable)
  x <- dplyr::left_join(x, wqbc_codes(), by = "Variable")
  stopifnot(all(x$Units == x$..Units))
  x$..Units <- NULL
  x
}

average_monthly_values <- function (x) {
  x <- plyr::ddply(x, "Variable", average_monthly_values_variable)
  x
}

join_limits <- function (x) {
  x$..ID <- 1:nrow(x)
  x <- dplyr::left_join(x, wqbc_limits(), by = c("Variable", "Units"))
  x
}

get_code_values <- function (x) {
  code_value <-  as.list(x$Value)
  names(code_value) <- x$Code
  code_value[!duplicated(names(code_value))]
}

test_condition <- function (x, cvalues) {
  if(is.na(x))
    return (TRUE)
  x <- try(eval(parse(text = x), envir = cvalues), silent = TRUE)
  if(class(x) != "logical")
    return (FALSE)
  return (x)
}

calc_limit <- function (x, cvalues) {
  x <- try(eval(parse(text = as.character(x)), envir = cvalues), silent = TRUE)
  if(class(x) != "numeric")
    return (NA)
  return (x)
}

calc_limits_by_period <- function (x) {
  cvalues <- get_code_values(x)

  x$Condition <- vapply(x$Condition, FUN = test_condition,
                        FUN.VALUE = logical(1), cvalues = cvalues)

  x <- x[x$Condition,,drop = FALSE]
  x$Condition <- NULL
  x$UpperLimit <- vapply(x$UpperLimit, FUN = calc_limit,
                         FUN.VALUE = numeric(1), cvalues = cvalues)

  x[!is.na(x$UpperLimit),,drop = FALSE]
}

get_conditional_codes <- function (x) {
  x <- stringr::str_extract_all(x, "EMS_[[:alnum:]][[:alnum:]_]{3,3}")
  unique(unlist(x))
}

average_conditional_code_values <- function (x) {
  txt <- paste0("x$Value <- ", x$Average[1], "(x$Value)")
  eval(parse(text = txt))
  x[1,,drop = FALSE]
}

fill_in_conditional_codes <- function (x, ccodes) {
  y <- dplyr::filter_(x, ~Code %in% ccodes)
  y <- plyr::ddply(y, .variables = "Code", .fun = average_conditional_code_values)

  if(nrow(y)) {
    dates <- expand.grid(Date = unique(x$Date), Variable = unique(y$Variable),
                         stringsAsFactors = FALSE)
    y <- dplyr::left_join(dates, y, by = c("Date", "Variable"))
    y <- dplyr::anti_join(y, x, by = c("Date", "Variable"))
    x <- rbind(x, y)
  }
  x
}

calc_limits_by_date <- function (x, messages) {

  ccodes <- get_conditional_codes(x$Condition[x$Term == "Short"])
  x <- dplyr::filter_(x, ~(!is.na(Term) & Term == "Short") | Code %in% ccodes)
  x <- fill_in_conditional_codes(x, ccodes)
  x <- plyr::ddply(x, "Date", calc_limits_by_period)
  stopifnot(!anyDuplicated(x$..ID))
  x
}

calc_limits_by_30day <- function (x) {
  x <- dplyr::filter_(x, ~!is.na(Term) & Term == "Short")
#  x$Year <- lubridate::year(x$Date)
 # x$Month <- lubridate::month(x$Date)

  x <- plyr::ddply(x, c("Year", "Month"), average_monthly_values)
  x <- plyr::ddply(x, c("Year", "Month"), calc_limits_by_period)
  x <- dplyr::filter_(x, ~Values >= 5 & Weeks >= 3)

  if(!nrow(x))
    return (x)

  x$Date <- as.Date(paste(x$Year, x$Month, "01", sep = "-"))

  stopifnot(!anyDuplicated(x$..ID))

  x <- dplyr::select_(x, ~-..ID, ~-Code, ~-Average, ~-Year, ~-Month, ~-Values, ~-Weeks)
  x
}

calc_limits_by <- function (x, term, messages) {
  x <- join_codes(x)
  x <- join_limits(x)

  #  if(term == "long") {
  #    x <- calc_limits_by_30day(x)
  #  } else {
  x <- calc_limits_by_date(x, messages = messages)
  #  }
  x <- dplyr::select_(x, ~Date, ~Variable, ~Value, ~UpperLimit, ~Units, ~Term)
  x
}

#' Calculates Water Quality limits
#'
#' Calculates the approved upper water quality thresholds for
#' British Columbia.
#'
#' @inheritParams calc_wqis
#' @param term A string indicating whether to calculate long-term or short-term limits.
#' @examples
#' \dontrun{
#' demo(fraser, ask = FALSE)
#' }
#' @export
calc_limits <- function (x, by = NULL, term = "long",
                         messages = getOption("wqbc.messages", default = TRUE),
                         parallel = getOption("wqbc.parallel", default = FALSE)) {

  assert_that(is.data.frame(x))
  assert_that(is.null(by) || (is.character(by) && noNA(by)))
  assert_that(is.string(term))
  assert_that(is.flag(messages) && noNA(messages))
  assert_that(is.flag(parallel) && noNA(parallel))

  term <- tolower(term)
  if(!term %in% c("long", "short")) stop("term must be \"long\" or \"short\"")

  x <- clean_wqdata(x, by = by, messages = messages)

  if(messages) message("Calculating water quality limits...")

  if(is.null(by)) {
    x <- calc_limits_by(x, term = term, messages = messages)
  } else {
    x <- plyr::ddply(x, .variables = by, .fun = calc_limits_by, .parallel = parallel,
                term = term, messages = messages)
  }
  if(messages) message("Calculated.")
  x
}
