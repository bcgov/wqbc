require(devtools)
require(tidyr)
require(dplyr)
require(magrittr)

rm(ccme)
ccme <- read.csv("data-raw/ccme.csv", na.strings = c("NA", ""), stringsAsFactors = FALSE)
ccme$Date <- as.Date(ccme$Date)
ccme <- tidyr::gather(ccme, "Code", "Value", -Date)
limits <- dplyr::filter(ccme, Date %in% as.Date(c("0001-01-01", "1111-11-11")))
ccme %<>% dplyr::filter(!Date %in% as.Date(c("0001-01-01", "1111-11-11")))
ccme$DetectionLimit <- ccme$Value
ccme$Value %<>% sub("^L", "", .) %>% as.numeric
is.na(ccme$DetectionLimit[!grepl("^L", ccme$DetectionLimit)]) <- TRUE
ccme$DetectionLimit %<>% sub("^L", "", .) %>% as.numeric

limits %<>% spread(Date, Value)
colnames(limits) <- c("Code", "LowerLimit", "UpperLimit")

ccme %<>% dplyr::inner_join(limits, by = "Code") %>% dplyr::filter(!is.na(Date))
ccme$LowerLimit %<>% as.numeric
ccme$UpperLimit %<>% as.numeric
ccme %<>% filter(!is.na(Value))
ccme$Code <- as.factor(as.character(ccme$Code), levels = unique(ccme$Code))

summary(ccme)
devtools::use_data(ccme, overwrite = TRUE, compress = "xz")
