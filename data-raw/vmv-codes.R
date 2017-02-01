library(dplyr)
library(magrittr)
library(readr)
library(devtools)

vmv_codes <- read_csv("http://donnees.ec.gc.ca/data/substances/monitor/national-long-term-water-quality-monitoring-data/Water-Qual-Eau-VariableInfo.csv",
                      locale = locale(encoding = "latin1"))

vmv_codes %<>% select(Variable = VARIABLE, VMV_Code = VMV_CODE, Variable_Code = VARIABLE_CODE)

vmv_codes %>%  distinct() %>% arrange(Variable) %>% filter(!is.na(Variable))

vmv_codes %<>% as.tbl()
use_data(vmv_codes, overwrite = TRUE)
