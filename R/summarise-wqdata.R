se <- function(x, na.rm) {
  stats::sd(x, na.rm = na.rm) / sqrt(sum(!is.na(x)))
}

summarise_wqdata_by <- function(x, level, na.rm) {
  x <- x$Value

  # necessary as quantiles errors if missing values and na.rm = FALSE
  if(any(is.na(x)) && !na.rm) {
    lowerQ <- NA_real_
    upperQ <- NA_real_
  } else {
    lowerQ <- stats::quantile(x, (1-level)/2, na.rm = na.rm, names = FALSE)
    upperQ <- stats::quantile(x, level + (1-level)/2, na.rm = na.rm, names = FALSE)
  }

  tibble::tibble(
    n = if(na.rm) sum(!is.na(x)) else length(x),
    min = min(x, na.rm = na.rm),
    max = max(x, na.rm = na.rm),
    mean = mean(x, na.rm = na.rm),
    median = stats::median(x, na.rm = na.rm),
    lowerQ = lowerQ,
    upperQ = upperQ,
    sd = stats::sd(x, na.rm = na.rm),
    se = se(x, na.rm = na.rm))
}

summarise_norows <- function(integer) {
  value0 <- if(integer) integer(0) else double(0)
  tibble::tibble(
    n = integer(0),
    min = value0,
    max = value0,
    mean = double(0),
    median = value0, # because sometimes median can be integer
    lowerQ = double(0),
    upperQ = double(0),
    sd = double(0),
    se = double(0))
}

summarise_wqdata_norows <- function(x, by) {
  dplyr::bind_rows(x[by], summarise_norows(is.integer(x$Value)))
}

#' Summarise Water Quality Data
#'
#' Summarises water quality data in the Values (numeric) column
#' by Variable (character or factor) column.
#'
#' Provides basic summary statistics (n, min, max, mean, median, se, sd).
#' SD; Options for quantiles, levels, upper/lower confidence intervals). View summary statistics in a tab of the results panel (added function to wqbc)
#'
#' @param x The data.frame to summarise.
#' @param by A character vector of the columns in x to summarise by.
#' @param na.rm A flag specifying whether to exclude missing values when summarising.
#' @param level A number between 0 and 1 specifying the upper and lower percentiles.
#' @return A tibble of the summary statistics.
#' @export
#' @examples
#' data.frame(Variable = "var", Value = 1:5, stringsAsFactors = FALSE)
summarise_wqdata <- function(x, by = NULL, level = 0.95, na.rm = FALSE) {
  chk_data(x)
  chk_subset(colnames(x), c("Variable", "Value"))
  chk_numeric(x$Value)
  chk_character_or_factor(x$Variable)
  check_by(by, colnames(x), res_names = c("Value"))
  chk_number(level)
  chk_range(level)
  chk_flag(na.rm)

  by <- c("Variable", by)
  by <- unique(by)

  if(!nrow(x)) {
    x <- summarise_wqdata_norows(x, by = by)
  } else {
    x <- plyr::ddply(x, .variables = by, .fun = summarise_wqdata_by,
                     level = level, na.rm = na.rm)
  }
  tibble::as_tibble(x)
}
