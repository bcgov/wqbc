wqbc_limits <- function () {
  limits <- limits
  limits$Variable <- as.character(limits$Variable)
  limits$Units <- as.character(limits$Units)
  limits
}

join_codes <- function (x) {
  x <- dplyr::rename_(x, "..Units" = "Units")
  x$Variable <- as.character(x$Variable)
  x <- dplyr::left_join(x, wqbc_codes(), by = "Variable")
  stopifnot(all(x$Units == x$..Units))
  x$..Units <- NULL
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
  x$Conditional <- FALSE
  if(nrow(y)) {
    dates <- expand.grid(Date = unique(x$Date), Variable = unique(y$Variable),
                         stringsAsFactors = FALSE)
    y$Date <- NULL
    y <- dplyr::inner_join(dates, y, by = "Variable")
    y <- dplyr::anti_join(y, x, by = c("Date", "Variable"))
    if(nrow(y)) {
      y$Conditional <- TRUE
      x <- rbind(x, y)
    }
  }
  x
}

calc_limits_by_date <- function (x) {
  ccodes <- get_conditional_codes(x$Condition[x$Term == "Short"])
  x <- dplyr::filter_(x, ~(!is.na(Term) & Term == "Short") | Code %in% ccodes)
  x <- fill_in_conditional_codes(x, ccodes)
  x <- plyr::ddply(x, "Date", calc_limits_by_period)
  x <- dplyr::filter_(x, ~Term == "Short")
  x <- dplyr::filter_(x, ~!Conditional)
  stopifnot(!anyDuplicated(x$..ID))
  x
}

abs_days_diff <- function (x, y) {
  abs(as.integer(diff(c(x, y))))
}

assign_30day_periods <- function (x, dates) {
  dates <- unique(dates)
  y <- unique(dplyr::select_(x, ~Date))
  y <- dplyr::arrange_(y, ~Date)
  is.na(y$Period) <- NA

  period <- 1
  start_date <- y$Date[1]
  y$Period[1] <- period

  if(nrow(y) > 1) {
    for(i in 2:nrow(y)) {
      if(abs_days_diff(start_date, y$Date[i]) > 30 | y$Date[i] %in% dates) {
        period <- period + 1
        start_date <- y$Date[i]
      }
      y$Period[i] <- period
    }
  }
  x <- dplyr::left_join(x, y, by = "Date")
  stopifnot(noNA(x$Period))
  x$Period <- factor(x$Period)
  x
}

average_30day_values_variable <- function (x) {
  stopifnot(!is.unsorted(x$Date))
  x$Samples <- nrow(x)
  if(!is.null(x$DetectionLimit))
    x$DetectionLimit <- mean(x$DetectionLimit)

  x$Span <- abs_days_diff(x$Date[1], x$Date[x$Samples])
  txt <- paste0("x$Value <- ", x$Average[1], "(x$Value)")
  eval(parse(text = txt))
  x[1,,drop = FALSE]
}

average_30day_values <- function (x) {
  plyr::ddply(x, c("Variable", "Condition"), average_30day_values_variable)
}

calc_limits_by_30day <- function (x, dates) {
  ccodes <- get_conditional_codes(x$Condition[x$Term == "Long"])
  x <- dplyr::filter_(x, ~(!is.na(Term) & Term == "Long") | Code %in% ccodes)
  x <- dplyr::arrange_(x, ~Date)

  x <- assign_30day_periods(x, dates)
  x <- plyr::ddply(x, c("Period"), average_30day_values)
  x <- fill_in_conditional_codes(x, ccodes)
  x <- plyr::ddply(x, "Date", calc_limits_by_period)
  x <- dplyr::filter_(x, ~Term == "Long")
  x <- dplyr::filter_(x, ~!Conditional)
  x <- dplyr::filter_(x, ~Samples >= 5 & Span >= 21)
  stopifnot(!anyDuplicated(x$..ID))
  x
}

calc_limits_by <- function (x, term, dates) {
  x <- join_codes(x)
  x <- join_limits(x)

  if(term == "long") {
    x <- calc_limits_by_30day(x, dates)
  } else {
    x <- calc_limits_by_date(x)
  }
  if(!is.null(x$DetectionLimit)) {
    x <- dplyr::select_(x, ~Date, ~Variable, ~Value, ~UpperLimit, ~DetectionLimit, ~Units)
  } else
    x <- dplyr::select_(x, ~Date, ~Variable, ~Value, ~UpperLimit, ~Units)
  x
}

#' Calculates Water Quality limits
#'
#' Calculates the approved upper water quality thresholds for
#' British Columbia.
#'
#' @param x The data.frame to perform the calculations on.
#' @param by A character vector of the columns to perform the calculations by.
#' @param term A string indicating whether to calculate long-term or short-term limits.
#' @param dates A date vector indicating the start of each 30 day period.
#' @param messages A flag indicating whether to print messages.
#' @examples
#' \dontrun{
#' demo(fraser)
#' }
#' @export
calc_limits <- function (x, by = NULL, term = "long", dates = NULL,
                         messages = getOption("wqbc.messages", default = TRUE)) {

  assert_that(is.data.frame(x))
  assert_that(is.null(by) || (is.character(by) && noNA(by)))
  assert_that(is.string(term))
  assert_that(is.null(dates) || (is.date(dates) && noNA(dates)))
  assert_that(is.flag(messages) && noNA(messages))

  term <- tolower(term)
  if(!term %in% c("long", "short")) stop("term must be \"long\" or \"short\"")

  x <- clean_wqdata(x, by = by, messages = messages)

  if(messages) message("Calculating ", paste0(term, "-term") ," water quality limits...")

  if(is.null(by)) {
    x <- calc_limits_by(x, term = term, dates = dates)
  } else {
    x <- plyr::ddply(x, .variables = by, .fun = calc_limits_by,
                     term = term, dates = dates)
  }
  if(messages) message("Calculated ", paste0(term, "-term") ," water quality limits.")
  x
}
