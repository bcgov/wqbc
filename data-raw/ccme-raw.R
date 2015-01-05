require(devtools)
require(tidyr)
require(dplyr)
require(magrittr)

rm(ccme_raw)
ccme_raw <- read.csv("data-raw/ccme-raw.csv", na.strings = c("NA", ""), stringsAsFactors = FALSE)
summary(ccme_raw)
devtools::use_data(ccme_raw, overwrite = TRUE, compress = "xz")

ccme <- tidyr::gather(ccme_raw, "Code", "Value", -Date)
gline <- dplyr::filter(ccme, is.na(Date)) %>% select(Code, Guideline = Value)

ccme <- dplyr::inner_join(ccme, gline, by = "Code") %>% dplyr::filter(!is.na(Date))
summary(ccme)
devtools::use_data(ccme, overwrite = TRUE, compress = "xz")
