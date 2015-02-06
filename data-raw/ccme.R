library(devtools)

ccme <- read.csv("data-raw/ccme.csv", na.strings = c("NA", ""), stringsAsFactors = FALSE)
ccme$Date <- as.Date(ccme$Date)
ccme$Variable <- factor(as.character(ccme$Variable), levels = unique(ccme$Variable))
ccme$Units <- factor(ccme$Units)
devtools::use_data(ccme, overwrite = TRUE, compress = "xz")
