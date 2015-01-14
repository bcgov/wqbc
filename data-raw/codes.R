library(wqbc)
library(dplyr)
library(magrittr)
library(devtools)

rm(list = ls())
graphics.off()

input_codes <- function () {

  codes <- read.csv("data-raw/codes.csv", na.strings = c("NA", ""), stringsAsFactors = FALSE)
  stopifnot(identical(colnames(codes), c("Variable","Code","Units")))
  stopifnot(all(!is.na(codes$Code)))
  stopifnot(all(!is.na(codes$Variable)))
  stopifnot(all(!is.na(codes$Units)))

  stopifnot(!anyDuplicated(codes$Code))
  stopifnot(!anyDuplicated(codes$Variable))
  stopifnot(all(codes$Units %in% get_units()))

  codes$Variable %<>% factor
  codes$Code %<>% factor
  codes$Units %<>% factor(levels = get_units())
  codes$Units %<>% droplevels

  codes
}
codes <- input_codes()
devtools::use_data(codes, overwrite = TRUE, compress = "xz")
