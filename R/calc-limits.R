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
  x <- dplyr::rename(x, "..Units" = "Units")
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

get_code_values <- function(x) {
  code_value <- as.list(x$Value)
  names(code_value) <- x$Code
  code_value[!duplicated(names(code_value))]
}

test_condition <- function(x, cvalues) {
  if (is.na(x)) {
    return(TRUE)
  }
  x <- try(eval(parse(text = x), envir = cvalues), silent = TRUE)
  if (class(x) != "logical") {
    return(FALSE)
  }
  return(x)
}

calc_limit <- function(x, cvalues) {
  x <- try(eval(parse(text = as.character(x)), envir = cvalues), silent = TRUE)
  if (class(x) != "numeric") {
    return(NA)
  }
  return(x)
}

calc_limits_by_period <- function(x) {
  cvalues <- get_code_values(x)

  x$Condition <- vapply(x$Condition,
    FUN = test_condition,
    FUN.VALUE = logical(1), cvalues = cvalues
  )

  x <- x[x$Condition, , drop = FALSE]
  x$Condition <- NULL
  x$UpperLimit <- vapply(x$UpperLimit,
    FUN = calc_limit,
    FUN.VALUE = numeric(1), cvalues = cvalues
  )

  x[!is.na(x$UpperLimit), , drop = FALSE]
}

get_conditional_codes <- function(x) {
  x <- stringr::str_extract_all(x, "EMS_[[:alnum:]][[:alnum:]_]{3,3}")
  x <- unique(unlist(x))
  x <- x[!is.na(x)]
  x
}

average_conditional_code_values <- function(x) {
  txt <- paste0("x$Value <- ", x$Average[1], "(x$Value)")
  eval(parse(text = txt))
  x[1, , drop = FALSE]
}

fill_in_conditional_codes <- function(x, ccodes) {
  y <- dplyr::filter(x, .data$Code %in% ccodes)
  y <- plyr::ddply(y, .variables = "Code", .fun = average_conditional_code_values)
  x$Conditional <- FALSE
  if (nrow(y)) {
    dates <- expand.grid(
      Date = unique(x$Date), Variable = unique(y$Variable),
      stringsAsFactors = FALSE
    )
    y$Date <- NULL
    y <- dplyr::inner_join(dates, y, by = "Variable")
    y <- dplyr::anti_join(y, x, by = c("Date", "Variable"))
    if (nrow(y)) {
      y$Conditional <- TRUE
      x <- rbind(x, y)
    }
  }
  x
}

calc_limits_by_date <- function(x, term, messages) {
  ccodes <- get_conditional_codes(x$Condition[x$Term == term])

  dropped <- dplyr::filter(x, !((!is.na(.data$Term) & .data$Term == term) | .data$Code %in% ccodes))
  x %<>% dplyr::filter(((!is.na(.data$Term) & .data$Term == term) | .data$Code %in% ccodes))

  if (!nrow(x)) {
    return(NULL)
  }

  x %<>% fill_in_conditional_codes(ccodes)

  x <- plyr::ddply(x, "Date", calc_limits_by_period)
  x <- dplyr::filter(x, .data$Term == term)
  x <- dplyr::filter(x, !.data$Conditional)
  stopifnot(!anyDuplicated(x$..ID))
  x
}

abs_days_diff <- function(x, y) {
  abs(as.integer(diff(c(x, y))))
}

assign_30day_periods <- function(x, dates) {
  if (!is.null(dates)) dates <- sort(unique(dates))
  y <- unique(dplyr::select(x, .data$Date))
  y <- dplyr::arrange(y, .data$Date)
  y$Period <- NA

  period <- 1
  start_date <- y$Date[1]
  y$Period[1] <- period

  if (nrow(y) > 1) {
    for (i in 2:nrow(y)) {
      if (abs_days_diff(start_date, y$Date[i]) > 30 | y$Date[i] %in% dates) {
        period <- period + 1
        start_date <- y$Date[i]
      }
      y$Period[i] <- period
    }
  }
  x <- dplyr::left_join(x, y, by = "Date")
  chk_not_any_na(x$Period)
  x$Period <- factor(x$Period)
  x
}

average_30day_values_variable <- function(x) {
  stopifnot(!is.unsorted(x$Date))
  x$Samples <- nrow(x)
  if (!is.null(x$DetectionLimit)) {
    x$DetectionLimit <- mean(x$DetectionLimit)
  }

  x$Span <- abs_days_diff(x$Date[1], x$Date[x$Samples])
  txt <- paste0("x$Value <- ", x$Average[1], "(x$Value)")
  eval(parse(text = txt))
  x[1, , drop = FALSE]
}

average_30day_values <- function(x) {
  plyr::ddply(x, c("Variable", "Condition"), average_30day_values_variable)
}

calc_limits_by_30day <- function(x, dates, messages) {
  ccodes <- get_conditional_codes(x$Condition[x$Term == "Long"])

  dropped <- dplyr::filter(x, !(!is.na(.data$Term) & .data$Term == "Long") | .data$Code %in% ccodes)
  x %<>% dplyr::filter((!is.na(.data$Term) & .data$Term == "Long") | .data$Code %in% ccodes)

  if (messages) {
    dropped %<>% dplyr::group_by(.data$Variable) %>% dplyr::summarise_(n = length(.data$Variable))
    for (i in seq_along(dropped$Variable)) {
      message(
        "Dropped ", sum(dropped$n[i]),
        " values without limits for ", dropped$Variable[i], "."
      )
    }
  }

  if (!nrow(x)) {
    return(NULL)
  }

  x <- dplyr::arrange(x, .data$Date)

  x <- assign_30day_periods(x, dates)
  x <- plyr::ddply(x, c("Period"), average_30day_values)
  x <- fill_in_conditional_codes(x, ccodes)
  x <- plyr::ddply(x, "Date", calc_limits_by_period)
  x <- dplyr::filter(x, .data$Term == "Long")
  x <- dplyr::filter(x, !.data$Conditional)
  x <- dplyr::filter(x, .data$Samples >= 5 & .data$Span >= 21)
  stopifnot(!anyDuplicated(x$..ID))
  x
}

calc_limits_by <- function(x, term, dates, limits, messages) {
  x <- join_codes(x)
  if ("Code.y" %in% names(x)) {
    x <- dplyr::rename(x, Code = "Code.y")
  }
  x <- join_limits(x, limits)

  if (term == "long") {
    x <- calc_limits_by_30day(x, dates, messages)
  } else if (term == "short") {
    x <- calc_limits_by_date(x, "Short", messages)
  } else { # term == "long-daily"
    x <- calc_limits_by_date(x, "Long", messages)
  }
  if (is.null(x)) {
    return(NULL)
  }

  if (!is.null(x$DetectionLimit)) {
    x <- dplyr::select(x, .data$Date, .data$Variable, .data$Value, .data$UpperLimit, .data$DetectionLimit, .data$Units)
  } else {
    x <- dplyr::select(x, .data$Date, .data$Variable, .data$Value, .data$UpperLimit, .data$Units)
  }
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
#' @param clean Should the data be run through \code{\link{clean_wqdata}} before calculating limits? Default \code{TRUE}
#' @param limits A data frame of the limits table to use.
#' @param use A string indicating the Use.
#' @param messages A flag indicating whether to print messages.
#' @examples
#' \dontrun{
#' demo(fraser)
#' }
#' @seealso \code{\link{clean_wqdata}} and \code{\link{lookup_limits}}
#' @export
calc_limits <- function(x, by = NULL, term = "long", dates = NULL, keep_limits = TRUE,
                        delete_outliers = FALSE, estimate_variables = FALSE,
                        clean = TRUE, limits = wqbc::limits,
                        messages = getOption("wqbc.messages", default = TRUE),
                        use = "Freshwater Life") {
  chk_data(x)
  chkor(chk_null(by), check_values(by, ""))
  chk_string(term)
  chk_subset(term, c("long", "short", "long-daily"))
  chkor(chk_null(dates), check_values(dates, Sys.Date()))
  chk_flag(keep_limits)
  chk_flag(delete_outliers)
  chk_flag(estimate_variables)
  chk_flag(clean)
  check_limits(limits)
  chk_flag(messages)
  chk_string(use)
  chk_subset(use, c(unique(limits$Use), rep("Freshwater Life", 2)))

  missing_limits <- dplyr::anti_join(limits, wqbc::codes, by = c("Variable", "Units"))

  if (nrow(missing_limits)) {
    error("unrecognised variables and/or units in limits - they must match those in wqbc::codes")
  }
  limits$Code <- NULL

  limits <- dplyr::filter(limits, .data$Use == use)
  limits$Use <- NULL

  if (keep_limits) {
    bol <- rep(FALSE, nrow(x))
    if (tibble::has_name(x, "LowerLimit")) {
      bol <- !is.na(x$LowerLimit)
    }
    if (tibble::has_name(x, "UpperLimit")) {
      bol <- bol | !is.na(x$UpperLimit)
    }

    y <- dplyr::slice(x, which(bol))
    x %<>% dplyr::slice(which(!bol))
  }

  if (clean) {
    x %<>% clean_wqdata(by = by, delete_outliers = delete_outliers, messages = messages)
  }

  x <- standardize_wqdata(x, messages = messages)

  cleansed <- x

  x_org <- dplyr::filter(x, .data$Variable %in% estimated_variables())

  if (estimate_variables) {
    x %<>% estimate_variable_values(by = by, messages = messages)
  }

  x$DetectionLimit <- NULL

  if (messages) message("Calculating ", paste0(term, "-term"), " water quality limits...")

  if (is.null(by)) {
    x <- calc_limits_by(x, term = term, dates = dates, limits = limits, messages = messages)
  } else {
    x <- plyr::ddply(x,
      .variables = by, .fun = calc_limits_by,
      term = term, dates = dates, limits = limits, messages = messages
    )
  }

  if (estimate_variables) { ## add original variable values back if still present
    x_new <- dplyr::filter(x, .data$Variable %in% estimated_variables())
    x %<>% dplyr::filter(!.data$Variable %in% estimated_variables())
    x_org <- x_org[c("Date", "Variable", by, "Value")]
    x_new$Value <- NULL
    x_new %<>% dplyr::inner_join(x_org, by = c("Date", "Variable", by))
    x %<>% dplyr::bind_rows(x_new)
  }

  if (messages) {
    cleansed %<>% dplyr::anti_join(x, by = c("Date", "Variable", by))

    if (nrow(cleansed)) {
      cleansed %<>% plyr::ddply("Variable", function(x) data.frame(n = nrow(x)))
      for (i in seq_len(nrow(cleansed))) {
        message("Dropped ", cleansed$n[i], " values for ", cleansed$Variable[i], " without limits")
      }
    } else {
      message("O values without limits")
    }

    message("Calculated ", paste0(term, "-term"), " water quality limits.")
  }
  if (keep_limits) {
    x %<>% dplyr::bind_rows(y)
  }
  tibble::as_tibble(x)
}
