se <- function(x, na.rm) {
  stats::sd(x, na.rm = na.rm) / sqrt(sum(!is.na(x)))
}

summarise_wqdata_by <- function(x, na.rm) {
  x <- x$Value
  tibble::tibble(
    n = if(na.rm) sum(!is.na(x)) else length(x),
    min = min(x, na.rm = na.rm),
    max = max(x, na.rm = na.rm),
    mean = mean(x, na.rm = na.rm),
    median = stats::median(x, na.rm = na.rm),
    sd = stats::sd(x, na.rm = na.rm),
    se = se(x, na.rm = na.rm))
}

summarise_norows <- function() {
  tibble::tibble(
    n = integer(0),
    min = double(0),
    max = double(0),
    mean = double(0),
    median = double(0),
    sd = double(0),
    se = double(0))
}

summarise_wqdata_norows <- function(x, by) {
  dplyr::bind_rows(x[by], summarise_norows())
}

#' Summarise Water Quality Data
#'
#' Summarises water quality data in the Values (numeric) column
#' by Variable (character or factor) column.
#'
#' Provides basic summary statistics (n, min, max, mean, median, se, sd).
#' SD; Options for quantiles, percentiles, upper/lower confidence intervals). View summary statistics in a tab of the results panel (added function to wqbc)
#'
#' @param x The data.frame to summarise.
#' @param by A character vector of the columns in x to summarise by.
#' @param na.rm A flag specifying whether to exclude missing values when summarising.
#' @return A tibble of the summary statistics.
#' @export
#' @examples
#' data.frame(Variable = "var", Value = 1:5, stringsAsFactors = FALSE)
summarise_wqdata <- function(x, by = NULL, na.rm = FALSE) {
  chk_data(x)
  chk_subset(colnames(x), c("Variable", "Value"))
  chk_numeric(x$Value)
  chk_character_or_factor(x$Variable)
  check_by(by, colnames(x), res_names = c("Value"))
  chk_flag(na.rm)

  by <- c("Variable", by)
  by <- unique(by)

  if(!nrow(x)) {
    x <- summarise_wqdata_norows(x, by = by)
  } else {
    x <- plyr::ddply(x, .variables = by, .fun = summarise_wqdata_by, na.rm = na.rm)
  }
  tibble::as_tibble(x)
}
