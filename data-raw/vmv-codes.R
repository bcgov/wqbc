library(dplyr)
library(magrittr)
library(readr)
library(devtools)
# library(tidyr)
# library(rems)

vmv_codes <- read_csv("http://donnees.ec.gc.ca/data/substances/monitor/national-long-term-water-quality-monitoring-data/Water-Qual-Eau-VariableInfo.csv",
                      locale = locale(encoding = "latin1"))

# vmv_codes %<>% select(VMV_CODE, VARIABLE_CODE, VARIABLE, VARIABLE_TYPE, UNIT_NAME,
#                       METHOD_CODE, METHOD_TITLE)
#
# ems_codes <- rems::ems_parameters
#
# vmv_ems <- read_csv("data-raw/VMV_to_EMS.csv")
#
# vmv_ems %<>%
#   filter(OTHER_SYSTEM_NAME == "EMS") %>%
#   separate(OTHER_SYSTEM_CODE, c("EMS_CODE", "EMS_METHOD_CODE"), 4) %>%
#   mutate(OTHER_SYSTEM_UNIT_CODE = as.character(OTHER_SYSTEM_UNIT_CODE))
#
# foo <- left_join(ems_codes, vmv_ems,
#                  by = c("PARAMETER_CODE" = "EMS_CODE",
#                         "ANALYTICAL_METHOD_CODE" = "EMS_METHOD_CODE",
#                         "UNIT_CODE" = "OTHER_SYSTEM_UNIT_CODE")) %>%
#   left_join(vmv_codes, by = c("vmv_code" = "VMV_CODE",
#                               "method_code" = "METHOD_CODE"))

vmv_codes %<>% select(Variable = VARIABLE, VMV_Code = VMV_CODE, EC_Code = VARIABLE_CODE)

vmv_codes %<>% distinct() %>% filter(!is.na(Variable)) %>% dplyr::arrange(Variable)

vmv_codes %<>% as.tbl()
use_data(vmv_codes, overwrite = TRUE)
