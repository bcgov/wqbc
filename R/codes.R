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

wqbc_codes <- function (compress = FALSE) {
  codes <- wqbc::codes
  codes$Code <- as.character(codes$Code)
  if(compress)
    codes$Code <- compress_ems_codes(codes$Code)
  codes$Variable <- as.character(codes$Variable)
  codes$Units <- as.character(codes$Units)
  codes
}

#' Compress EMS Codes
#'
#' Compresses EMS codes by removing EMS_ from
#' start and replacing all '_' with '-'. This function is provided
#' because wqbc stored EMS codes in expanded form.
#'
#' @param x A character vector of codes to compress.
#' @examples
#' compress_ems_codes(c("EMS_0014", "EMS_KR-P", "0-15"))
#' @seealso \code{\link{expand_ems_codes}}
#' @export
compress_ems_codes <- function (x) {
  assert_that(is.character(x) || is.factor(x))
  x <- as.character(x)
  x <- gsub("[_]", "-", x)
  sub("^EMS[-]", "", x)
}

#' Expand EMS Codes
#'
#' Expands EMS codes by adding EMS_ to start if absent
#' and replacing all '-' with '_'. This function is provided
#' because wqbc stored EMS codes in expanded form.
#'
#' @param x A character vector of codes to expand
#' @examples
#' expand_ems_codes(c("0014", "KR-P", "0_15", "EMS_ZN_T"))
#' @seealso \code{\link{compress_ems_codes}}
#' @export
expand_ems_codes <- function (x) {
  assert_that(is.character(x) || is.factor(x))
  x <- as.character(x)
  x <- gsub("[-]", "_", x)
  bol <- !grepl("^EMS[_-]", x)
  x[bol] <- paste0("EMS_", x[bol])
  x
}
