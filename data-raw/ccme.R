library(devtools)
library(tidyr)
library(dplyr)
library(magrittr)

ccme <- read.csv("data-raw/ccme.csv", na.strings = c("NA", ""), stringsAsFactors = FALSE)
ccme$Date <- as.Date(ccme$Date)
ccme <- tidyr::gather(ccme, "Variable", "Value", -Date)
limits <- dplyr::filter(ccme, Date %in% as.Date(c("0001-01-01", "1111-11-11")))
ccme %<>% dplyr::filter(!Date %in% as.Date(c("0001-01-01", "1111-11-11")))
ccme$Value %<>% sub("^L", "", .) %>% as.numeric

limits %<>% spread(Date, Value)
colnames(limits) <- c("Variable", "LowerLimit", "UpperLimit")

ccme %<>% dplyr::inner_join(limits, by = "Variable") %>% dplyr::filter(!is.na(Date))
ccme$LowerLimit %<>% as.numeric
ccme$UpperLimit %<>% as.numeric
ccme %<>% filter(!is.na(Value))
ccme$Variable <- gsub("[.]", " ", ccme$Variable)
ccme$Variable <- sub("X2 4 D", "2,4-D", ccme$Variable)

ccme$Variable <- factor(as.character(ccme$Variable), levels = unique(ccme$Variable))

summary(ccme)
devtools::use_data(ccme, overwrite = TRUE, compress = "xz")
