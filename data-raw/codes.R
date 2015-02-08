library(wqbc)
library(dplyr)
library(magrittr)
library(devtools)

rm(list = ls())

input_codes <- function () {

  codes <- read.csv("data-raw/codes.csv", na.strings = c("NA", ""), stringsAsFactors = FALSE)
  stopifnot(identical(colnames(codes), c("Variable","Code","Units", "Average")))

  stopifnot(all(!is.na(codes)))

  stopifnot(!anyDuplicated(codes$Code))
  stopifnot(!anyDuplicated(codes$Variable))
  stopifnot(all(codes$Units %in% lookup_units()))
  stopifnot(all(codes$Average %in% c("mean", "median")))


  codes$Code %<>%  factor
  codes$Variable %<>% factor
  codes$Units %<>% factor(levels = lookup_units())
  codes$Units %<>% droplevels
  codes$Average %<>% factor

  codes
}
codes <- input_codes()
devtools::use_data(codes, overwrite = TRUE, compress = "xz")
