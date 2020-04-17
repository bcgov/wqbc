se <- function(x, na.rm) {
  stats::sd(x, na.rm = na.rm) / sqrt(sum(!is.na(x)))
}

summarise_wqdata_by_cenmle <- function(x, level, na.rm, dist) {
  x$Censored <- is.na(x$Value) & !is.na(x$DetectionLimit) & x$Value <= x$DetectionLimit
  x$Value[x$Censored] <- x$DetectionLimit[x$Censored]
  ml <- with(x, cenmle(Value, Censored, dist = dist))

  tibble::tibble(
    n = if(na.rm) sum(!is.na(x$Value)) else nrow(x),
    min = min(x$Value, na.rm = na.rm),
    max = max(x$Value, na.rm = na.rm),
    mean = mean(ml)[["mean"]],
    median = median(ml),
    lowerQ = unname(quantile(ml,  (1-level)/2)),
    upperQ = unname(quantile(ml, level + (1-level)/2)),
    sd = sd(ml),
    se = mean(ml)[["se"]])
}

summarise_wqdata_by_stats <- function(x, level, na.rm) {
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

summarise_wqdata_by <- function(x, cenmle, level, na.rm, dist) {
  if(cenmle) return(summarise_wqdata_by_cenmle(x, level, na.rm, dist))
  summarise_wqdata_by_stats(x, level, na.rm)
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
#' By default provides summary statistics for water quality data in the Values (numeric) column
#' by Variable (character or factor) column.
#'
#' The user can specify additional columns to calculate the summary statistics by
#' and/or account for non-detects using left-censored maximum-likelihood estimation.
#' Missing values are considered to be below the detection limit (if the detection limit is defined).
#'
#' @param x The data.frame to summarise.
#' @param by A character vector of the columns in x to summarise by.
#' @param na.rm A flag specifying whether to exclude missing values when summarising.
#' @param cenmle A flag specifying whether to account for non-detects using left-censored maximum likelihood estimation.
#' @param level A number between 0 and 1 specifying the upper and lower percentiles.
#' @param dist Assumed distribution of the values for the left-censored maximum-likelihood model.
#' @param ... Additional arguments passed to \code{\link[survival]{survreg}}.
#' @return A tibble of the summary statistics.
#' @export
#' @examples
#' data.frame(Variable = "var", Value = 1:5, stringsAsFactors = FALSE)
summarise_wqdata <- function(x, by = NULL, cenmle = FALSE, level = 0.95, na.rm = FALSE,
                             dist = "lognormal") {
  chk_data(x)
  check_names(x, c("Variable", "Value"))
  chk_character_or_factor(x$Variable)
  chk_numeric(x$Value)
  chk_gte(x$Value)
  check_by(by, colnames(x), res_names = c("Value", "DetectionLimit"))
  chk_flag(cenmle)
  chk_number(level)
  chk_range(level)
  chk_flag(na.rm)
  chk_string(dist)
  chk_subset(dist, c("lognormal", "loglogistic", "weibull"))

  if(cenmle) {
    check_names(x, "DetectionLimit")
    chk_numeric(x$DetectionLimit)
    chk_gte(x$DetectionLimit)
  }

  by <- c("Variable", by)
  by <- unique(by)

  if(!nrow(x)) {
    x <- summarise_wqdata_norows(x, by = by)
  } else {
    x <- plyr::ddply(x, .variables = by, .fun = summarise_wqdata_by,
                     cenmle = cenmle, level = level, na.rm = na.rm,
                     dist = dist)
  }
  tibble::as_tibble(x)
}
