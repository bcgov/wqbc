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

check_rows <- function (x) {
  if(nrow(x) == 0) stop("x must contain at least one row of data")
  TRUE
}

check_columns <- function (x, colnames) {
  stopifnot(is.data.frame(x))
  stopifnot(is.character(colnames))

  colnames <- unique(colnames)

  bol <- colnames %in% colnames(x)
  if(!all(bol))
    stop("x must contain ", plural("column", sum(!bol) > 1, " "), punctuate_strings(colnames[!bol], "and"))
  TRUE
}

check_class_columns <- function (x, columns) {
  check_columns(x, names(columns))

  for(colname in names(columns)) {
    if(!class(x[[colname]]) %in% columns[[colname]])
      stop("column ", colname, " must be class ", punctuate_strings(columns[[colname]], "or"))
  }
  TRUE
}

check_by <- function(by, colnames, res_names = NULL) {
  if (is.null(by))
    return(TRUE)

  if (!all(by %in% colnames))
    stop("x must contain columns ", punctuate_strings(by, "and"), " in by")

  if (is.null(res_names)) return(TRUE)

  if (any(by %in% res_names))
    stop("by must not include ", punctuate_strings(res_names, "and"))
  TRUE
}

check_excursions <- function (x) {
  if(any(is.infinite(x$Excursion))) {
    vars <- unique(x$Variable[is.infinite(x$Excursion)])
    vars <- sort(vars)
    stop(plural("Variable", length(vars) > 1, " "), punctuate_strings(vars, "and"),
         " ", ifelse(length(vars) > 1,  "have", "has"),
         " a LowerLimit and one or more zero Values with no defined DetectionLimit")
  }
  TRUE
}

