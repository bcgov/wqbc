#' Compress EMS Codes
#'
#' Compresses EMS codes by removing EMS_ from
#' start and replacing all '_' with '-'.
#'
#' @param x A character vector of codes to compress.
#' @examples
#' compress_ems_codes(c("EMS_0014", "EMS_KR-P", "0-15"))
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
#' and replacing all '-' with '_'.
#'
#' @param x A character vector of codes to expand
#' @examples
#' expand_ems_codes(c("0014", "KR-P", "0_15", "EMS_ZN_T"))
#' @export
expand_ems_codes <- function (x) {
  assert_that(is.character(x) || is.factor(x))
  x <- as.character(x)
  x <- gsub("[-]", "_", x)
  bol <- !grepl("^EMS[_-]", x)
  x[bol] <- paste0("EMS_", x[bol])
  x
}

wqbc_codes <- function (compress = FALSE) {
  codes <- codes
  codes$Code <- as.character(codes$Code)
  if(compress)
    codes$Code <- compress_ems_codes(codes$Code)
  codes$Variable <- as.character(codes$Variable)
  codes$Units <- as.character(codes$Units)
  codes
}
