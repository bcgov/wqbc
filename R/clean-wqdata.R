cv <- function (x) {
  if(length(x) == 1)
    return (0)
  if(mean(x) == 0)
    return (0)
  sd(x) / mean(x)
}

abs_dev <- function (x) {
  abs(x - mean(x))
}

clean_wqdata_replicates <- function (x, max_cv, messages) {
  n <- nrow(x)
  cv <- cv(x$Value)
  if(cv(x$Value) > max_cv && nrow(x) > 2) {
    x <- dplyr::arrange_(x, ~-Value)
    while(cv(x$Value) > max_cv && nrow(x) > 2) {
      x <- x[-which.max(abs_dev(x$Value)),]
    }
  }
  x$Value <- mean(x$Value)

  if(messages && n > nrow(x)) {
    message("Filtered ", n - nrow(x), " of ", n,
            " replicate values with a CV of ", signif(cv, 3), " for ", x$Variable[1],
            " on ", x$Date[1], ".")
  }
  x[1,,drop = FALSE]
}

clean_wqdata_variable <- function (x, max_cv, messages) {
  if(anyDuplicated(x$Date))
    x <- plyr::ddply(x, "Date", clean_wqdata_replicates, max_cv = max_cv,
                     messages = messages)
  x
}

clean_wqdata_by <- function (x, max_cv, messages) {
  if(anyDuplicated(x$Variable))
    x <- plyr::ddply(x, "Variable", clean_wqdata_variable, max_cv = max_cv,
                     messages = messages)
  x
}

#' Clean Water Quality Data
#'
#' Cleans water quality data. After standardization replicates are averaged.
#' If there are three or more values with a coefficient of variation (CV) in
#' exceedance of \code{max_cv} then the value with the highest absolute deviation
#' is dropped until the CV is less than or equal to \code{max_cv}
#' or only two values remain. The default value max_cv value of 1.29
#' is exceeded by two zero and one positive value (CV = 1.73)
#' or by two identical positive values and a third value an order
#' or magnitude greater (CV = 1.30). It is not exceed by one zero
#' and two identical positive values (CV = 0.87).
#'
#' @inheritParams calc_wqis
#' @param max_cv A number indicating the maximum permitted coefficient
#' of variation for replicates.
#' @examples
#' clean_wqdata(wqbc::dummy, messages = TRUE)
#' @export
clean_wqdata <- function (x, by = NULL, max_cv = 1.29,
                          messages = getOption("wqbc.messages", default = TRUE)) {

  assert_that(is.data.frame(x))
  assert_that(is.null(by) || (is.character(by) && noNA(by)))
  assert_that(is.number(max_cv))
  assert_that(is.flag(messages) && noNA(messages))

  x <- standardize_wqdata(x, messages = messages)

  if(messages) message("Cleaning water quality data...")

  x <- add_missing_columns(x, list("Date" = as.Date("2000-01-01")), messages = messages)

  check_class_columns(x, list("Date" = "Date"))

  check_by(by, colnames(x), res_names = unique(c("Date", "Variable", "Value", "Units")))

  colnames <- colnames(x)[!colnames(x) %in% c("Date", "Variable", "Value", "Units", by)]
  x <- delete_columns(x, colnames, messages = FALSE)

  if(is.null(by)) {
    x <- clean_wqdata_by(x, max_cv = max_cv, messages = messages)
  } else {
    x <- plyr::ddply(x, .variables = by, .fun = clean_wqdata_by, max_cv = max_cv,
                     messages = messages)
  }
  message("Cleansed.")
  x
}
