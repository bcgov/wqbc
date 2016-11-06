# Copyright 2016 Province of British Columbia
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

# This script contains functions for identifying a certain type of outlier
# 
# The method is based on Hampels Mean Absolute Deviation approach for time series
#
# The method is a combination of two tecniques:
#  1. remove large outliers when compared to the MAD of the whole time series
#  2. 
#     a. fit a reasonably flexible time series model to identify 
#        trend in the data using a robust regression approach by iteratively reweighting
#     b. delete observations with very large residuals from the robust fit
#     c. repeat this procedure until all outliers have been removed.
#

# wrapper function, 
#   outlier_removal_by - calls the two base removal methods:
#      1) outlier_removal_mad
#      2) outlier_removal_robust_ts
outlier_removal_by <- function(wk, w_threshold=6, theshold=10, debug=FALSE) {

  # track how many points are being removed
  wk_orig <- wk
  ndata <- nrow(wk)  
  
  # do by mad - finds extreme values
  wk %<>% outlier_removal_mad(., threshold = theshold)
  ndata <- c(ndata, nrow(wk))
  
  if (ndata[1] - ndata[2] > 20 | ndata[2]/ndata[1] < 0.9) {
    # do not proceed! and return origional data
    warning(wk$Station_Number[1], " ", wk$Code[1], ": A large amount of data would be removed as outliers - please check!")
    return(wk_orig)
  }
    
  # then fit a time series model
  # monitor changes
  orig <- nrow(wk)
  change <- TRUE
  while(change) {
    # identify outliers and drop
    wk %<>% outlier_removal_robust_ts(., w_threshold = w_threshold, debug = debug)
    ndata <- c(ndata, nrow(wk))
    change <- nrow(wk) < orig
    orig <- nrow(wk)
  }

  # assess the removals?
  
  wk
}


# this function removes any observations that are more than 'threshold' mad's (calculated
#   on the log scale) away from the median
outlier_removal_mad <- function(wk, threshold = 10) {

  # if data is constant then no outliers
  if (nrow(wk) == 1 || sd(wk$Value) == 0) {
    return(wk)
  }
  
  if (mad(log(wk$Value)) == 0 && length(unique(wk$Value)) > 1) {
      # only warn if mad is 0 and values are not all the same
      # if values are all the same, then there are no outliers to remove
      # and setting madx = 0.2 has no consequence.
      message(wk$Station_Number[1], "-", wk$Code[1], ": Median absolute deviation is zero, increasing to 0.2.")
  }
    
  # first identify scale problems
  # work on the log scale as posotive observations tend to have constant CV behaviour
  mad_resids <- function(x) {
    x <- log(x)
    madx <- mad(x)
    if (madx == 0) {
      madx <- 0.2
    } 
    (x - median(x))/madx
  }
  # what threshold to choose?
  # 10 on the log scale?
  wk %<>% filter(mad_resids(Value) < threshold)
  
  # return cleaned data
  wk
}


# this function removed observations that are more than 'w_threshold' sd's (calculated 
# on the lg scale) from a timeseries model fitted to the log observations.
# 
# The model fitted is a GAM, with a seasonal and long term component.  It is iteratively
# refitted with weights based on the previous model fits residuals, much link the
# robust linear regression fitting algorithm in MASS.
outlier_removal_robust_ts <- function(wk,   w_threshold=6, debug=FALSE) {

  # make working copy
  wkcopy <- wk
  
  # if data is constant then no outliers
  if (nrow(wk) == 1 || sd(wk$Value) == 0) {
    return(wk)
  }
  
  if (debug) {
    cat("doing Station", wk$Station_Number[1], "Code", wk$Code[1], "\n")
  }
  
  # re-weighting function
  # type used in MASS
  w1 <- function(x, k) {
    ifelse(abs(x) <= k, 1, k / abs(x))
  }
  # bisquare weighting function i.e clevland
  w2 <- function(x, k) {
    ifelse(abs(x)>k, 0, (1 - x^2/k^2)^2)
  }
  # select MASS version for now,
  w <- w1
  
  # residual function
  w_resid <- function (mod) {
    # uing the prediction standard deviation
    # means we keep observations that are poorly predicted
    sd.y <- predict(mod, se.fit = TRUE)$se.fit + sqrt(mod$sig2)
    res <- (mod$y - fitted(mod)) / sd.y
    res
  }
  
  # select a transformation
  # note - log transform runs into trouble due to
  # bimodal nature of some data - e.g. 50% very small and 50% positive
  trans <- function(x) x^.5
  # in any case, it is still useful to replace zero values by half the minimum non-zero value
  # this is not generally a good thing for modelling, but we are only trying to define a method
  # to identify observations that are too big.
  if (any(wk$Value == 0)) {
    wk$Value[wk$Value == 0] <- min(wk$Value[wk$Value>0])/2
  }
  
  # initialise weights and convergence criteria
  weights <- rep(1, nrow(wk))
  change <- 1
  iterations <- 0
  # These could be generalised in a control list
  change_thresh <- 0.001
  iter_max <- 10
  
  # setup model
  ndays <- wk$Date %>% year %>% table
  nyears <- wk$Date %>% year %>% table %>% length
  nseasonal <- floor(mean(ndays))
  #nunique <- length(unique(wk$Value))
  
  # if too few data in each year, then this method won't work well
  if (all(ndays < 9)) {
    message(wk$Station_Number[1], "-", wk$Code[1], ": Too few data data to find outliers")
    return(wk)
  }
  
  # choose model
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
  while (change > change_thresh && iterations < iter_max) {
    iterations <- iterations + 1
    # fit a timeseries model
    mod <- mgcv::gam(formula, 
                     data = wk,
                     weights = weights)
    res <- w_resid(mod)
    # calculate re-weighting
    # weight using residuals with a cut off at w_threshold
    old_weights <- weights
    weights <- w(res, w_threshold)
    # do not downweight small values
    weights[res<0] <- 1
    change <- sum(abs(weights - old_weights))
  }
  
  # now drop residuals larger than w_threshold
  # we could set a differnt value here ...
  out <- wk %>% filter(res < w_threshold)
  
  # return cleaned data
  out
}

