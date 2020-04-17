summarise_norows <- function(x) {
  value0 <- if(is.integer(x$Value)) integer(0) else double(0)
  tibble::tibble(
    n = integer(0),
    ncen = integer(0),
    min = value0,
    max = value0,
    mean = double(0),
    median = double(0),
    lowerQ = double(0),
    upperQ = double(0),
    sd = double(0),
    se = double(0),
    lowerCL = double(0),
    upperCL = double(0))
}

summarise_missing_values <- function(x, censored) {
  NA_value_ <- if(is.integer(x$Value)) NA_integer_ else NA_real_
  ncen <- NA_integer_
  if(!censored) {
    ncen <- 0L
  } else if(all(is.na(x$Value) == is.na(x$DetectionLimit))) {
    ncen <- sum(!is.na(x$Value) & x$Value <= x$DetectionLimit)
  }
  tibble::tibble(
    n = nrow(x),
    ncen = ncen,
    min = NA_value_,
    max = NA_value_,
    mean = NA_real_,
    median = NA_real_,
    lowerQ = NA_real_,
    upperQ = NA_real_,
    sd = NA_real_,
    se = NA_real_,
    lowerCL = NA_real_,
    upperCL = NA_real_)
}

summarise_zero_values <- function(x, censored) {
  ncen <- if(!censored) 0L else sum(x$Value <= x$DetectionLimit)
  tibble::tibble(
    n = nrow(x),
    ncen = ncen,
    min = 0,
    max = max(x$Value),
    mean = NA_real_,
    median = NA_real_,
    lowerQ = NA_real_,
    upperQ = NA_real_,
    sd = NA_real_,
    se = NA_real_,
    lowerCL = NA_real_,
    upperCL = NA_real_)
}

summarise_wqdata_by <- function(x, group, censored, na.rm, conf_level, perc_range) {
  if(na.rm) x %<>% dplyr::filter(!is.na(.data$Value))
  if(!nrow(x)) {
    return(summarise_norows(x))
  }
  if(any(is.na(x$Value))) {
    return(summarise_missing_values(x, censored))
  }

  # get min and max before censored values altered
  min <- min(x$Value)
  max <- max(x$Value)

  if(!censored) {
    x$Censored <- FALSE
  } else {
    x$Censored <- !is.na(x$DetectionLimit) & x$Value <= x$DetectionLimit
    x$Value[x$Censored] <- x$DetectionLimit[x$Censored]
  }
  if(any(x$Value == 0)) {
    return(summarise_zero_values(x, censored))
  }
  ml <- with(x, cenmle(Value, Censored, dist = "lognormal", conf.int = conf_level))

  est <- mean(ml)
  quantiles <- quantile(ml,  c((1-perc_range)/2, perc_range + (1-perc_range)/2))

  tibble::tibble(
    n = nrow(x),
    ncen = sum(x$Censored),
    min = min,
    max = max,
    mean = est[["mean"]],
    median = median(ml),
    lowerQ = quantiles[[1]],
    upperQ = quantiles[[2]],
    sd = sd(ml),
    se = est[["se"]],
    lowerCL = est[[3]],
    upperCL = est[[4]])
}

summarise_wqdata_norows <- function(x, by) {
  dplyr::bind_rows(x[by], summarise_norows(x))
}

#' Summarise Water Quality Data
#'
#' Calculates summary statistics for water quality data
#' using log-normal maximum-likelihood models.
#'
#' The data set must include a numeric 'Value' and
#' a character or factor 'Variable' column.
#'
#' By default the summary statistics are independently calculated for each Variable.
#' The user can specify additional columns to independently calculate the statistics by using the by argument.
#'
#' If the user wishes to calculate the statistics for groups assuming the same variance
#' then they should specify them using the group argument.
#' A column cannot be specified in both the by and group argument.
#'
#' If the user wishes to account for non-detects using left-censored maximum-likelihood
#' (by setting censored = TRUE) the data set must also include a numeric DetectionLimit column.
#'
#' Missing values in the DetectionLimit column are assumed to indicate that the
#' Values are not censored.
#' Missing values in the Value column are always considered to be missing values.
#' If the user wishes to exclude missing values in the Value column
#' they should set na.rm = TRUE.
#'
#' @param x The data.frame to summarise.
#' @param by A character vector specifying the columns in x to independently summarise by.
#' @param group A character vector specifying the columns in x to summarise by assuming the same variance.
#' @param censored A flag specifying whether to account for non-detects.
#' @param na.rm A flag specifying whether to exclude missing Value values when summarising.
#' @param conf_level A number between 0 and 1 specifying confidence limits.
#' @param perc_range A number between 0 and 1 specifying the quantile range.
#' @return A tibble of the summary statistics.
#' @export
#' @examples
#' data.frame(Variable = "var", Value = 1:5, stringsAsFactors = FALSE)
summarise_wqdata <- function(x, by = NULL, group = NULL, censored = FALSE,
                             na.rm = FALSE, conf_level = 0.95, perc_range = 0.95) {
  chk_data(x)
  check_names(x, c("Variable", "Value"))
  chk_character_or_factor(x$Variable)
  chk_numeric(x$Value)
  chk_gte(x$Value)
  check_by(by, colnames(x), res_names = c("Value", "DetectionLimit"))
  check_by(group, colnames(x), res_names = c("Value", "Variable", "DetectionLimit", by))
  chk_flag(censored)
  chk_flag(na.rm)
  chk_number(conf_level)
  chk_range(conf_level)
  chk_number(perc_range)
  chk_range(perc_range)

  if(censored) {
    check_names(x, "DetectionLimit")
    chk_numeric(x$DetectionLimit)
    chk_gte(x$DetectionLimit)
  }

  by <- unique(c("Variable", by))
  group <- unique(group)

  if(!nrow(x)) {
    x <- summarise_wqdata_norows(x, by = by)
  } else {
    x <- plyr::ddply(x, .variables = by, .fun = summarise_wqdata_by,
                     group = group, censored = censored, na.rm = na.rm,
                     conf_level = conf_level,
                     perc_range = perc_range)
  }
  tibble::as_tibble(x)
}
