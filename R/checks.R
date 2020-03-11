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

check_class_columns <- function (x, columns) {
  check_names(x, names(columns))

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

check_limits <- function(limits) {

  check_names(limits, c("Variable", "Use", "Term", "Condition", "UpperLimit", "Units"))

  limits$Variable %<>% as.character()
  limits$Term %<>% as.character()
  limits$Condition %<>% as.character()
  limits$Units %<>% as.character()
  limits$Use %<>% as.character()

  check_data(limits, values = list(Variable = lookup_variables(),
                                   Use = lookup_use(),
                                    Term = c("Short", "Long", "Long"),
                                    Condition = c("", NA),
                                    UpperLimit = "",
                                    Units = lookup_units()))
  limits
}
