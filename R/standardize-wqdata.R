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

standardize_wqdata_variable <- function (x, messages) {
  codes <- wqbc_codes()
  codes <- dplyr::filter_(codes, ~Variable == x$Variable[1])
  x$Value <- convert_values(x$Value, from = x$Units, to = codes$Units,
                            messages = messages)
  if(!is.null(x$DetectionLimit)) {
    x$DetectionLimit <- convert_values(x$DetectionLimit, from = x$Units,
                                       to = codes$Units, messages = messages)
  }
  x$Units <- codes$Units
  x
}

#' Standardize Water Quality Data
#'
#' Standardizes a water quality data set using \code{\link{substitute_variables}}
#' and \code{\link{substitute_units}} so that all remaining values have the
#' recognised codes and variables in \code{\link{codes}} and the standard
#' units. If column Code is present then a Variable column is created using
#' \code{\link{lookup_variables}}. The \code{standardize_wqdata} function
#' is called by \code{clean_wqdata} prior to cleaning.
#'
#' @inheritParams substitute_variables
#' @param x A data.frame to standardize.
#' @param strict A flag that is passed to substitute_variables indicating
#' whether to require all words in a recognised variable name to be
#' present in x (strict = TRUE) or only the first one (strict = FALSE)
#' @examples
#' standardize_wqdata(wqbc::dummy, messages = TRUE)
#' @seealso \code{\link{clean_wqdata}}
#' @aliases standardise_wqdata
#' @export
standardize_wqdata <- function (
  x, strict = TRUE, messages = getOption("wqbc.messages", default = TRUE)) {
  assert_that(is.data.frame(x))
  assert_that(is.flag(strict) && noNA(strict))
  assert_that(is.flag(messages) && noNA(messages))

  check_rows(x)

  if("Code" %in% colnames(x)) {
    if(messages) message ("Converting Codes to Variables...")
    # x$Variable <- lookup_variables(x$Code, messages = messages)
    # x <- delete_rows_with_certain_values(
    #   x, columns = c("Variable"), messages = messages)
    if(messages) message ("Converted Codes to Variables.")
  }

  if(messages) message("Standardizing water quality data...")

  check_columns(x, c("Variable", "Value", "Units"))

  check_class_columns(x, list("Variable" = c("character", "factor"),
                              "Value" = "numeric",
                              "Units" = c("character", "factor")))

  # x <- delete_rows_with_certain_values(x, columns = c("Variable", "Value", "Units"),
  #                                      messages = messages, txt = "missing")
  #
  # x <- delete_rows_with_certain_values(x, columns = "Value",
  #                                      messages = messages, txt = "negative")

  if(!nrow(x)) { if(messages) message("Standardized water quality data."); return (x) }

  # x$Variable <- substitute_variables(x$Variable, strict = strict, messages = messages)
  # is.na(x$Variable[!x$Variable %in% lookup_variables()]) <- TRUE
  #
  # x <- delete_rows_with_certain_values(x, columns = c("Variable"),
  #                                      messages = messages)
  #
  # x$Units <- substitute_units(x$Units, messages = messages)
  # is.na(x$Units[!x$Units %in% lookup_units()]) <- TRUE
  #
  # x <- delete_rows_with_certain_values(x, columns = c("Units"),
  #                                      messages = messages)

  if(!nrow(x)) { if(messages) message("Standardized water quality data."); return (x) }

  x <- plyr::ddply(x, .variables = "Variable",
                   .fun = standardize_wqdata_variable, messages = messages)

  if(messages) message("Standardized water quality data.")
  x
}
