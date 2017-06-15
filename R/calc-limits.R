# Copyright 2015 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

wqbc_limits <- function(limits = wqbc::limits) {
  limits$Variable %<>% as.character()
  limits$Units %<>% as.character()
  limits
}

join_codes <- function(x) {
  x <- dplyr::rename_(x, "..Units" = "Units")
  x$Variable %<>% as.character()
  x %<>% dplyr::left_join(wqbc_codes(), by = "Variable")
  stopifnot(all(x$Units == x$..Units))
  x$..Units <- NULL
  x
}

join_limits <- function(x, limits = wqbc::limits) {
  x$..ID <- seq_len(nrow(x))
  x %<>% dplyr::left_join(wqbc_limits(limits), by = c("Variable", "Units"))
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
  x <- unique(unlist(x))
  x <- x[!is.na(x)]
  x
}

average_conditional_code_values <- function (x) {
  txt <- paste0("x$Value <- ", x$Average[1], "(x$Value)")
  eval(parse(text = txt))
  x[1,,drop = FALSE]
}

fill_in_conditional_codes <- function(x, ccodes) {
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

calc_limits_by_date <- function(x, term, messages) {

  ccodes <- get_conditional_codes(x$Condition[x$Term == term])

  dropped <- dplyr::filter_(x, ~!((!is.na(Term) & Term == term) | Code %in% ccodes))
  x %<>% dplyr::filter_(~((!is.na(Term) & Term == term) | Code %in% ccodes))

  if (!nrow(x))
    return(NULL)

  x %<>% fill_in_conditional_codes(ccodes)

  x <- plyr::ddply(x, "Date", calc_limits_by_period)
  x <- dplyr::filter_(x, ~Term == term)
  x <- dplyr::filter_(x, ~!Conditional)
  stopifnot(!anyDuplicated(x$..ID))
  x
}

abs_days_diff <- function (x, y) {
  abs(as.integer(diff(c(x, y))))
}

assign_30day_periods <- function (x, dates) {
  if (!is.null(dates)) dates <- sort(unique(dates))
  y <- unique(dplyr::select_(x, ~Date))
  y <- dplyr::arrange_(y, ~Date)
  y$Period <- NA

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

calc_limits_by_30day <- function(x, dates, messages) {
  ccodes <- get_conditional_codes(x$Condition[x$Term == "Long"])

  dropped <- dplyr::filter_(x, ~!((!is.na(Term) & Term == "Long") | Code %in% ccodes))
  x %<>% dplyr::filter_(~((!is.na(Term) & Term == "Long") | Code %in% ccodes))

  if (messages) {
    dropped %<>% dplyr::group_by_(~Variable) %>% dplyr::summarise_(n = ~n())
    for (i in seq_along(dropped$Variable)) {
      message("Dropped ", sum(dropped$n[i]),
              " values without limits for ", dropped$Variable[i], ".")
    }
  }

  if (!nrow(x)) return(NULL)

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

calc_limits_by <- function (x, term, dates, limits, messages) {
  x <- join_codes(x)
  if ("Code.y" %in% names(x)) {
    x <- dplyr::rename_(x, Code = "Code.y")
  }
  x <- join_limits(x, limits)

  if (term == "long") {
    x <- calc_limits_by_30day(x, dates, messages)
  } else if (term == "short") {
    x <- calc_limits_by_date(x, "Short", messages)
  } else {# term == "long-daily"
    x <- calc_limits_by_date(x, "Long", messages)
  }
  if (is.null(x)) return(NULL)

  if (!is.null(x$DetectionLimit)) {
    x <- dplyr::select_(x, ~Date, ~Variable, ~Value, ~UpperLimit, ~DetectionLimit, ~Units)
  } else
    x <- dplyr::select_(x, ~Date, ~Variable, ~Value, ~UpperLimit, ~Units)
  x
}

#' Calculate Limits
#'
#' Calculates the approved "short" or "long"-term
#' upper water quality thresholds for freshwater life in British Columbia.
#' The water quality data is automatically cleaned using \code{\link{clean_wqdata}}
#' prior to calculating the limits to ensure: all variables are recognised,
#' all values are non-negative and in the standard units, divergent replicates
#' are filtered and all remaining replicates are averaged. Only limits whose
#' conditions are met are returned.
#' Note to use the long-term limits on daily values set
#' term = "long-daily".
#'
#' If a limit depends on another variable
#' such as pH, Total Chloride, or Total Hardness and no value was recorded for the date of interest
#' then the pH, Total Chloride or Total Hardness value is assumed to be the
#' average recorded value over the 30 day period.
#' The one exception is if \code{estimate_variables = TRUE} in which case a parametric model
#' is used to predict the pH, Total Chloride and Total Hardness for all dates with a value of any variable.
#' Existing values are replaced.
#' If, in every year, there are less then 12 pH/Total Chloride/Total Hardness then an average value is taken.
#' Otherwise, if there is only one year with 12 or more values a simple seasonal smoother is used.
#' If there is two years with 12 or more values then a seasonal smoother with a trend is fitted.
#' Otherwise a model with trend and a dynamic seasonal component is fitted.
#'
#' When considering long-term limits there must be at least 5 values
#' spanning 21 days. As replicates are averaged prior to calculating the limits
#' each of the 5 values must be on a separate day. The first 30 day period
#' begin at the date of the first reading while the next 30 day period
#' starts at the date of the first reading after the previous period and so on.
#' The only exception to this is if the user provides dates in which case each
#' period extends for 30 days or until a provided date is reached. It is important
#' to note that the averaging of conditional variables, the 5 in 30 rule
#' and the assignment of 30 day periods occurs independently for all combination
#' of factor levels in the columns specified by by.
#'
#'
#' @param x A data.frame of water quality readings to calculate the limits for.
#' @param by A optional character vector of the columns in x to calculate the limits by.
#' @param term A string indicating whether to calculate the "long" or "short"-term limits.
#' @param dates A optional date vector indicating the start of 30 day long-term periods.
#' @param keep_limits A flag indicating whether to keep values with user supplied upper or lower limits.
#' @param delete_outliers A flag indicating whether to delete outliers or merely flag them.
#' @param estimate_variables A flag indicating whether to estimate total hardness, total chloride and pHfor all dates.
#' @param limits A data frame of the limits table to use.
#' @param messages A flag indicating whether to print messages.
#' @examples
#' \dontrun{
#' demo(fraser)
#' }
#' @seealso \code{\link{calc_wqi}}, \code{\link{clean_wqdata}} and \code{\link{lookup_limits}}
#' @export
calc_limits <- function(x, by = NULL, term = "long", dates = NULL, keep_limits = TRUE,
                        delete_outliers = FALSE, estimate_variables = FALSE,
                        limits = wqbc::limits,
                        messages = getOption("wqbc.messages", default = TRUE)) {

  assert_that(is.data.frame(x))
  assert_that(is.null(by) || (is.character(by) && noNA(by)))
  assert_that(is.string(term))
  assert_that(is.null(dates) || (is.date(dates) && noNA(dates)))
  assert_that(is.flag(messages) && noNA(messages))
  assert_that(is.flag(keep_limits) && noNA(keep_limits))

  check_limits(limits)

  missing_limits <- dplyr::anti_join(limits, wqbc::codes, by = c("Variable", "Units"))

  if (nrow(missing_limits)) {
    error("unrecognised variables and/or units in limits - they must match those in wqbc::codes")
  }
  limits$Code <- NULL

  term <- tolower(term)
  if (!term %in% c("long", "short", "long-daily")) stop("term must be \"long\" or \"short\" or \"long-daily\"")

  if (keep_limits) {
    bol <- rep(FALSE, nrow(x))
    if (tibble::has_name(x, "LowerLimit"))
      bol <- !is.na(x$LowerLimit)
    if (tibble::has_name(x, "UpperLimit"))
      bol <- bol | !is.na(x$UpperLimit)

    y <- dplyr::slice_(x, ~which(bol))
    x %<>% dplyr::slice_(~which(!bol))
  }

  x %<>% clean_wqdata(by = by, delete_outliers  = delete_outliers, messages = messages)
  x <- standardize_wqdata(x, messages = messages)

  cleansed <- x

  x_org <- dplyr::filter_(x, ~Variable %in% c("Chloride Total", "Hardness Total", "pH"))

  if (estimate_variables)
    x %<>% estimate_variable_values(by = by, messages = messages)

  x$DetectionLimit <- NULL

  if (messages) message("Calculating ", paste0(term, "-term") ," water quality limits...")

  if (is.null(by)) {
    x <- calc_limits_by(x, term = term, dates = dates, limits = limits, messages = messages)
  } else {
    x <- plyr::ddply(x, .variables = by, .fun = calc_limits_by,
                     term = term, dates = dates, limits = limits, messages = messages)
  }

  if (estimate_variables) { ## add original variable values back if still present
    x_new <- dplyr::filter_(x, ~Variable %in% c("Chloride Total", "Hardness Total", "pH"))
    x %<>% dplyr::filter_(~!Variable %in% c("Chloride Total", "Hardness Total", "pH"))
    x_org <- x_org[c("Date", "Variable", by, "Value")]
    x_new$Value <- NULL
    x_new %<>% dplyr::inner_join(x_org, by = c("Date", "Variable", by))
    x %<>% dplyr::bind_rows(x_new)
  }

  if (messages) {
    cleansed %<>% dplyr::anti_join(x, by = c("Date", "Variable", by))

    if (nrow(cleansed)) {
      cleansed %<>% plyr::ddply("Variable", function(x) data.frame(n = nrow(x)))
      for (i in seq_len(nrow(cleansed)))
        message("Dropped ", cleansed$n[i], " values for ", cleansed$Variable[i]," without limits")
    } else
      message("O values without limits")

    message("Calculated ", paste0(term, "-term") ," water quality limits.")
  }
  if (keep_limits)
    x %<>% dplyr::bind_rows(y)
  x
}
