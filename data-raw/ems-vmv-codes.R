library(dplyr)
library(magrittr)
library(readr)
library(devtools)
library(tidyr)
library(rems)
library(canwqdata)

vmv_codes <- canwqdata::wq_params() %>%
        select(VMV_CODE,
               VMV_VARIABLE_CODE = VARIABLE_CODE,
               VMV_VARIABLE = VARIABLE,
               VMV_VARIABLE_TYPE = VARIABLE_TYPE,
               VMV_UNIT = UNIT_UNITÉ,
               VMV_UNIT_NAME = UNIT_NAME,
               VMV_METHOD_CODE = METHOD_CODE,
               VMV_METHOD_TITLE = METHOD_TITLE) %>%
        mutate_all(stringr::str_trim, side = "both") %>%
        distinct()

ems_codes <- rems::ems_parameters %>%
        select(EMS_CODE = PARAMETER_CODE,
               EMS_VARIABLE = PARAMETER,
               EMS_UNIT = UNIT,
               EMS_UNIT_CODE = UNIT_CODE,
               EMS_METHOD_CODE = ANALYTICAL_METHOD_CODE,
               EMS_METHOD_TITLE = ANALYTICAL_METHOD,
               EMS_MDL = METHOD_DETECTION_LIMIT) %>%
        mutate_all(stringr::str_trim, side = "both") %>%
        distinct()

## Ensure codes have consistent leading zeros etc.
vmv_ems_xwalk <- read_csv("data-raw/VMV_to_EMS.csv",
                          col_types = cols(.default = col_character())) %>%
        filter(OTHER_SYSTEM_NAME == "EMS") %>%
        separate(OTHER_SYSTEM_CODE, c("EMS_CODE", "EMS_METHOD_CODE"), 4) %>%
        select(VMV_CODE = vmv_code,
               VMV_METHOD_CODE = method_code,
               EMS_CODE,
               EMS_METHOD_CODE,
               EMS_UNIT_CODE = OTHER_SYSTEM_UNIT_CODE
               ) %>%
        mutate_all(stringr::str_trim, side = "both") %>%
        distinct()

# VMV_CODE should be unique but include VMV_METHOD_CODE for completeness.
# The combination of EMS_CODE, EMS_METHOD_CODE, and EMS_UNIT_CODE should be
# unique.
vmv_ems <- left_join(ems_codes, vmv_ems_xwalk,
                     by = c("EMS_CODE",
                            "EMS_METHOD_CODE",
                            "EMS_UNIT_CODE")) %>%
        left_join(vmv_codes, by = c("VMV_CODE", "VMV_METHOD_CODE")) %>%
        select(
                EMS_CODE,
                EMS_VARIABLE,
                EMS_UNIT,
                EMS_UNIT_CODE,
                EMS_METHOD_CODE,
                EMS_METHOD_TITLE,
                EMS_MDL,
                VMV_CODE,
                VMV_VARIABLE_CODE,
                VMV_VARIABLE,
                VMV_VARIABLE_TYPE,
                VMV_UNIT,
                VMV_UNIT_NAME,
                VMV_METHOD_CODE,
                VMV_METHOD_TITLE
        )

use_data(ems_codes, vmv_ems, vmv_codes, overwrite = TRUE)
