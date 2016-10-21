library(rems)
library(dplyr)
library(magrittr)
library(devtools)

try(download_historic_data())
ems_historic <- read_historic_data()
ems_current <- get_ems_data()

ems_codes <- bind_rows(ems_current, ems_historic)

ems_codes %<>% select(Variable = PARAMETER, Code = PARAMETER_CODE) %>%
  unique() %>% arrange(Variable) %>% filter(!is.na(Variable))

ems_codes %<>% as.tbl()
use_data(ems_codes, overwrite = TRUE, compress = "xz")
