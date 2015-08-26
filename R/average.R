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

#' Geometric Mean Plus-Minus 1
#'
#' Calculates the geometric mean by adding 1 before logging
#' and subtracting 1 before exponentiating so that provides results
#' even with zero counts. Not used by any wqbc functions but provided
#' as may be helpful if averaging bacterial counts.
#' @param x A numeric vector of non-negative numbers.
#' @param na.rm A flag indicating whether to remove missing values.
#' @examples
#' mean(0:9)
#' geomean1(0:9)
#' @export
geomean1 <- function (x, na.rm = FALSE) {
  assert_that(is.vector(x))
  assert_that(is.flag(na.rm) && noNA(na.rm))
  x <- as.numeric(x)

  if(any(x < 0, na.rm = TRUE))
    stop("x must not be negative")

  expm1(mean(log1p(as.numeric(x)), na.rm = na.rm))
}
