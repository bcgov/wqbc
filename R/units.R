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

get_unit_multiplier <- function(x) {
  units <- c("ng/L" = 10^-9, "ug/L" = 10^-6, "mg/L" = 10^-3,
             "g/L" = 1,  "kg/L" = 10^3,
             "pH" = 1,
             "degC" = 1, "C" = 1, "NTU" = 1,
             "CU" = 1, "Col.Unit" = 1, "Rel" = 1,
             "CFU/dL" = 1, "CFU/100mL" = 1, "CFU/mL" = 0.01, "MPN/dL" = 1,
             "MPN/100mL" = 1, "MPN/mL" = 0.01, "CFU/g" = 0.01, "MPN/g" = 0.01)
  x <- units[x]
  names(x) <- NULL
  x
}

get_unit_type <- function(x) {
  type <- list("concentration" = c("ng/L", "ug/L", "mg/L", "g/L", "kg/L"),
               "pH" = "pH",
               "Colour" = c("CU", "Col.Unit", "Rel"),
               "Temperature" = c("degC", "C"),
               "Turbidity" = "NTU",
               "Coli" = c("CFU/dL", "CFU/100mL", "CFU/mL", "MPN/dL", "MPN/100mL", "MPN/mL", "CFU/g", "MPN/g"))

  type <- unlist(type)
  names <- sub("\\d$", "", names(type))
  values <- type
  type <- names
  names(type) <- values

  x <- type[x]
  names(x) <- NULL
  x
}

#' Convert values to different units
#'
#' @param x a numeric vector of values to convert
#' @param from units to convert from
#' @param to units to convert to
#' @param messages should messages be printed when
#'
#' @details Currently supported units for \code{from} and \code{to} are:
#' c("ng/L", "ug/L", "mg/L", "g/L", "kg/L", "pH", "degC", "C", "CFU/dL", "MPN/dL", "CFU/100mL", "MPN/100mL", "CFU/g", "MPN/g", "CFU/mL", "MPN/mL", "CU", "Col.Unit", "Rel", "NTU")
#'
#' @return a numeric vector of the converted values
#' @export
#'
#' @examples
#'
#' convert_values(1, "ug/L", "mg/L", messages = FALSE)
#'
#' df <- data.frame(value = c(1.256, 5400000, 12300, .00098),
#'                  units = c("mg/L", "ng/L", "ug/L", "g/L"),
#'                  stringsAsFactors = FALSE)
#' df
#'
#' df$units_mg_L <- convert_values(df$value, from = df$units, to = "mg/L", messages = FALSE)
#' df
convert_values <- function (x, from, to, messages) {

  from <- substitute_units(from, messages = messages)
  to <- substitute_units(to, messages = messages)

  x <- x * get_unit_multiplier(from) / get_unit_multiplier(to)

  bol <- from != to & get_unit_type(from) != get_unit_type(to)
  bol <- is.na(bol) | bol

  if(any(bol)) {
    warning(sum(bol), " values have inconvertible units")
    is.na(x[bol]) <- TRUE
  }
  x
}
