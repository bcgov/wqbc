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


# re-weighting function
# type used in MASS
weights_mass <- function(x, k) {
  ifelse(abs(x) <= k, 1, k / abs(x))
}

# bisquare weighting function i.e clevland
weights_bisq <- function(x, k) {
  ifelse(abs(x)>k, 0, (1 - x^2/k^2)^2)
}

# residual function
#' @importFrom stats predict
#' @importFrom stats fitted
w_resid <- function (mod) {
  # uing the prediction standard deviation
  # means we keep observations that are poorly predicted
  sd.y <- predict(mod, se.fit = TRUE)$se.fit + sqrt(mod$sig2)
  res <- (mod$y - fitted(mod)) / sd.y
  unname(res)
}

# need to think of a better name for this
outlier_sense_check <- function(x) {

    # first subset out values already identified as outliers
  id_ok <- which(!x$is_outlier)
  if (length(id_ok)  == 0) {
    # no non outlier data left
    return(FALSE)
  }
  
  # if there are only a two unique values then we can't really say which are outliers
  if (length(unique(x$Value[id_ok])) < 3) {
    return(FALSE)
  }
  
  TRUE
}


# this function removes any observations that are more than 'threshold' mad's (calculated
#   on the log scale) away from the median
#' @importFrom stats mad
#' @importFrom stats median
#' @importFrom stats sd
outlier_id_mad <- function(x, threshold, max_cv, messages) {

  # check that it is sensible to look for outliers
  if (!outlier_sense_check(x)) {
    return(x)
  }
  
  # first subset out values already identified as outliers
  id_ok <- which(!x$is_outlier)

  # calculate robust summaries
  mad_x <- mad(x$Value[id_ok])
  centered_x <- (x$Value[id_ok] - median(x$Value[id_ok]))
  if (mad_x == 0) {
    # set the mad to something sensible ?
    if (messages) message(x$Station_Number[id_ok][1], "-", x$Code[id_ok][1], ": Median absolute deviation is zero, using stdev instead")
    mad_x <- sd(x$Value) # because of the sense checks sd(x) > 0
  }

  # identify outliers - note this is one sided, only too large values are removed.
  x$is_outlier[id_ok] <- (centered_x[id_ok]/mad_x) > threshold
  
  # return cleaned data
  x
}



# this function removed observations that are more than 'w_threshold' sd's (calculated 
# on the lg scale) from a timeseries model fitted to the log observations.
# 
# The model fitted is a GAM, with a seasonal and long term component.  It is iteratively
# refitted with weights based on the previous model fits residuals, much link the
# robust linear regression fitting algorithm in MASS.
#' @importFrom mgcv gam
#' @importFrom stats as.formula
#' @importFrom lubridate year
#' @importFrom lubridate yday
#' @importFrom lubridate decimal_date
outlier_id_robust_ts <- function(x, threshold, messages) {
  
  # check that it is sensible to look for outliers
  if (!outlier_sense_check(x)) {
    return(x)
  }
  
  # first subset out values already identified as outliers
  id_ok <- which(!x$is_outlier)
  
  # select robust iterative reweighting function
  weights_fun <- weights_mass
  
  # select a transformation
  # note - log transform runs into trouble due to
  # bimodal nature of some data - e.g. 50% very small and 50% positive
  trans <- function(x) x^.5
  inv_trans <- function(x) x^2
  # in the future could allow dofferent transformations, 
  # so could check this just in case? assertthat .. sum(abs(inv_trans(trans(1:10)) - 1:10)) < 1e-9

  # in any case, it is still useful to replace zero values by half the minimum non-zero value
  # this is not generally a good thing for modelling, but we are only trying to define a method
  # to identify observations that are too big.
  tol <- 1e-6
  if (any(x$Value < tol)) {
    x$Value[x$Value < tol] <- min(x$Value[x$Value>=tol])/2
  }
  
  # initialise weights
  weights <- rep(1, nrow(x))
  weights[!id_ok] <- 0
  
  # setup model
  ndays <- table(year(x$Date))
  nyears <- length(ndays)
  nseasonal <- floor(mean(ndays))
  nunique <- length(unique(x$Value))
  
  # if too few data in each year, then this method won't work well
  if (all(ndays < 9)) {
    if (messages) message(x$Station_Number[1], "-", x$Code[1], ": Too few data data to find outliers")
    return(x)
  }
  
  # choose maximum degrees of freedom for model
  ns <- min(6, floor(nseasonal/2))
  nt <- min(9, ceiling(nyears/2)+1)
  formula <- 
    if (nyears == 1) {
      sprintf("trans(Value) ~ s(decimal_date(Date), m=1, k=%i)", ns)
    } else if (nyears == 2) {
      sprintf("trans(Value) ~ s(yday(Date), bs='cc', m=1, k=%i) + decimal_date(Date)", ns)
    } else {
      sprintf("trans(Value) ~ s(yday(Date), bs='cc', m=1, k=%i) + s(decimal_date(Date), m=1, k=%i)", ns, nt)
    }
  formula <- as.formula(formula)
  
  # redo until weights stabalise
  # or too many iterations
  change <- TRUE
  iterations <- 0
  # These could be generalised in a control list
  change_thresh <- 0.001
  iter_max <- 10
  while (change > change_thresh && iterations < iter_max) {
    iterations <- iterations + 1
    # fit a timeseries model
    mod <- mgcv::gam(formula, data = x, weights = weights)
    res <- w_resid(mod)
    # calculate re-weighting
    # weight using residuals with a cut off at threshold,
    # but only set weights for those observations not already removed
    old_weights <- weights
    weights[weights>0] <- weights_fun(res[weights>0], threshold)
    # do not downweight small values
    weights[weights>0][res[weights>0]<0] <- 1
    change <- sum(abs(weights - old_weights))
  }
  
  # now drop residuals larger than w_threshold
  # we could set a differnt value here ...
  # but remember to maintian previously set outliers
  x$is_outlier[id_ok] <- res[id_ok] > threshold
  
  # tag on final model fit
  x$robust_ts_fit <- inv_trans(fitted(mod))
  
  # return cleaned data
  x
}


# wrapper function, 
#   outlier_removal_by - calls the two base removal methods:
#      1) outlier_removal_mad
#      2) outlier_removal_robust_ts
outlier_id_by <- function(x, mad_threshold, ts_threshold, max_cv, messages) {
  
  if (getOption("wqbc.debug", default = FALSE)) {
    cat("doing Station", x$Station_Number[1], "Code", x$Code[1], "\n")
  }
  
  n_outlier_start <- sum(x$is_outlier)
  
  # find extreme values
  x <- outlier_id_mad(x, threshold = mad_threshold, messages = messages)

  # then fit a time series model
  # monitor changes
  n_outlier <- nrow(x)+1 # to get things going
  while(sum(x$is_outlier) < n_outlier) {
    # save number of outliers for comparison
    n_outlier <- sum(x$is_outlier)
    # identify outliers
    x <- outlier_id_robust_ts(x, threshold = ts_threshold, messages = messages)
  }

  n_outlier_end <- sum(x$is_outlier)
  
  # assess the removals
  if (n_outlier_end - n_outlier_start > 20 | 
      (nrow(x) - n_outlier_end)/ (nrow(x) - n_outlier_start) < 0.5) {
    # warn
    if (messages) message(x$Station_Number[1], " ", x$Code[1], ": A large amount of data identified as outliers - please check!")
  }
  
  x
}



#' Identify Outliers In Water Quality Data
#'
#' Identifies outliers in water quality data.
#'
#' @details The method is motivated by Hampels Mean Absolute Deviation approach for time series. It is a 
#' combination of two tecniques:
#'   1. remove large outliers when compared to the MAD of the whole time series
#'   2. 
#'     a. fit a reasonably flexible time series model to identify 
#'        trend in the data using a robust regression approach by iteratively reweighting
#'     b. assign zero weight to observations with very large residuals from the robust fit
#'     c. repeat this procedure until all outliers have been identified.
#'
#' @param x The data.frame to analyse.
#' @param by A character vector of the columns in x to perform the outlier detection by.
#' @param mad_threshold A number indicating the maximum permitted coefficient
#' @param ts_threshold A number indicating the maximum permitted coefficient
#' @param max_cv A number indicating the maximum permitted coefficient
#' of variation.
#' @param messages A flag indicating whether to print messages.
#' @examples
#' x <- standardize_wqdata(wqbc::dummy)
#' outlier_id(x, by = "Variable", messages = TRUE)
#' @seealso \code{\link{calc_limits}} and \code{\link{standardize_wqdata}}
#' @export
outlier_id <- function(x, by = NULL, mad_threshold=10, ts_threshold=6, max_cv = 1.29,
                            messages = getOption("wqbc.messages", default = TRUE)) {
  assert_that(is.data.frame(x))
  assert_that(is.null(by) || (is.character(by) && noNA(by)))
  assert_that(is.number(mad_threshold))
  assert_that(is.number(ts_threshold))
  assert_that(is.number(max_cv))
  assert_that(is.flag(messages) && noNA(messages))
  
  x <- add_missing_columns(x, list("Date" = as.Date("2000-01-01"), "is_outlier" = FALSE), messages = messages)
  check_class_columns(x, list("Date" = "Date", "is_outlier" = "logical"))

  if(is.null(by)) {
    x <- outlier_id_by(x, 
                            mad_threshold = mad_threshold, ts_threshold = ts_threshold, max_cv = max_cv, 
                            messages = messages)
  } else {
    x <- plyr::ddply(x, .variables = by, .fun = outlier_id_by, 
                     mad_threshold = mad_threshold, ts_threshold = ts_threshold, max_cv = max_cv,
                     messages = messages)
  }
  if (messages) message("Looked for outliers in water quality data.")
  x
}
