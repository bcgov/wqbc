library(dplyr)
library(magrittr)
library(readr)
library(devtools)
library(tidyr)
# library(rems)
library(canwqdata)

vmv_codes <- canwqdata::wq_params()

vmv_codes %<>% select(VMV_CODE,
                      VMV_VARIABLE_CODE = VARIABLE_CODE,
                      VMV_VARIABLE = VARIABLE,
                      VMV_VARIABLE_TYPE = VARIABLE_TYPE,
                      VMV_UNIT = UNIT_UNITÉ,
                      VMV_UNIT_NAME = UNIT_NAME,
                      VMV_METHOD_CODE = METHOD_CODE,
                      VMV_METHOD_TITLE = METHOD_TITLE)

ems_codes <- rems::ems_parameters %>%
        select(
                EMS_CODE = PARAMETER_CODE,
                EMS_VARIABLE = PARAMETER,
                EMS_VARIABLE_ABBR = PARAMETER_ABBR,
                EMS_UNIT = UNIT,
                EMS_UNIT_CODE = UNIT_CODE,
                EMS_METHOD_CODE = ANALYTICAL_METHOD_CODE,
                EMS_METHOD_TITLE = ANALYTICAL_METHOD,
                EMS_MDL = METHOD_DETECTION_LIMIT
        )

vmv_ems <- read_csv("data-raw/VMV_to_EMS.csv",
                    col_types = cols(.default = col_character()))

vmv_ems %<>%
        filter(OTHER_SYSTEM_NAME == "EMS") %>%
        separate(OTHER_SYSTEM_CODE, c("EMS_CODE", "EMS_METHOD_CODE"), 4) %>%
        select(VMV_CODE = vmv_code,
               VMV_VARIABLE = variable_name,
               VMV_UNIT = unit_code,
               VMV_METHOD_CODE = method_code,
               EMS_CODE, EMS_METHOD_CODE,
               EMS_UNIT_CODE = OTHER_SYSTEM_UNIT_CODE) %>%
        distinct()

vmv_ems <- left_join(ems_codes, vmv_ems,
                     by = c("EMS_CODE",
                            "EMS_METHOD_CODE",
                            "EMS_UNIT_CODE")) %>%
        left_join(vmv_codes, by = c("VMV_CODE", "VMV_METHOD_CODE"))

vmv_codes %<>% select(Variable = VARIABLE, VMV_Code = VMV_CODE, EC_Code = VARIABLE_CODE)

vmv_codes %<>% distinct() %>% filter(!is.na(Variable)) %>% dplyr::arrange(Variable)

vmv_codes %<>% as.tbl()
use_data(vmv_codes, overwrite = TRUE)
