## NOTE THIS SCRIPT IS SOURCED BY data-raw/limits.R TO ENSURE codes is identical

library(dplyr)
library(magrittr)
library(devtools)

rm(list = ls())
graphics.off()

source("R/units.R")

input_codes <- function () {

  codes <- read.csv("data-raw/codes.csv", na.strings = c("NA", ""), stringsAsFactors = FALSE)

  stopifnot(identical(colnames(codes), c("Code", "Variable","Units")))
  stopifnot(all(!is.na(codes$Code)))
  stopifnot(all(!is.na(codes$Variable)))
  stopifnot(all(!is.na(codes$Units)))

  stopifnot(!anyDuplicated(codes$Code))
  stopifnot(!anyDuplicated(codes$Variable))
  stopifnot(all(codes$Units %in% get_units()))

  codes$Units %<>% factor(levels = get_units())
  codes$Units %<>% droplevels

  codes$Code %<>% factor
  codes$Variable %<>% factor

  codes %<>% arrange_(~Code)
  # write.csv(codes, "data-raw/codes.csv", row.names = FALSE)

  codes
}
codes <- input_codes()
summary(codes)
devtools::use_data(codes, overwrite = TRUE, compress = "xz")
