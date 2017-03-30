library(rems)
library(dplyr)
library(devtools)

ems_codes <- rems::ems_parameters %>%
  select(Variable = PARAMETER, Code = PARAMETER_CODE) %>%
  unique() %>% arrange(Variable) %>% filter(!is.na(Variable))

use_data(ems_codes, overwrite = TRUE, compress = "xz")
