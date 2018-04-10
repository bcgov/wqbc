library(dplyr)
library(magrittr)
library(readr)
library(devtools)
library(tidyr)
library(rems)
library(canwqdata)

camel_to_snake <- function(x, case = c("lower", "upper")) {
        case <- match.arg(case)
        case_fun <- switch(case, lower = tolower, upper = toupper)
        case_fun(gsub("([a-z0-9])([A-Z]+)", "\\1_\\2", x))
}

vmv_codes <- canwqdata::wq_params() %>%
        select(VMV_CODE,
               VMV_VARIABLE_CODE = NATIONAL_VARIABLE_CODE,
               VMV_VARIABLE = VARIABLE_COMMON_NAME,
               VMV_VARIABLE_TYPE = VARIABLE_TYPE,
               VMV_UNIT = MEASUREMENT_UNIT,
               VMV_UNIT_NAME = DESCRIPTION,
               VMV_METHOD_CODE = NATIONAL_METHOD_CODE,
               VMV_METHOD_TITLE = METHOD_TITLE) %>%
        mutate_all(stringr::str_trim, side = "both") %>%
        mutate_all(stringr::str_squish) %>%
        distinct()

vmv_reduced <- read_csv("data-raw/ec_variables_reduced.csv", trim_ws = TRUE) %>%
        rename_all(camel_to_snake, "upper") %>%
        mutate_all(stringr::str_trim, side = "both") %>%
        mutate_all(stringr::str_squish) %>%
        distinct()

vmv_codes <- vmv_codes %>%
        left_join(vmv_reduced, by = c("VMV_VARIABLE" = "VARIABLE_NAME"))

ems_codes <- rems::ems_parameters %>%
        select(EMS_CODE = PARAMETER_CODE,
               EMS_VARIABLE = PARAMETER,
               EMS_UNIT = UNIT,
               EMS_UNIT_CODE = UNIT_CODE,
               EMS_METHOD_CODE = ANALYTICAL_METHOD_CODE,
               EMS_METHOD_TITLE = ANALYTICAL_METHOD,
               EMS_MDL = METHOD_DETECTION_LIMIT) %>%
        mutate_all(stringr::str_trim, side = "both") %>%
        mutate_all(stringr::str_squish) %>%
        distinct()

## Ensure codes have consistent leading zeros etc.
vmv_ems_xwalk <- read_csv("data-raw/VMV_to_EMS.csv",
                          col_types = cols(.default = col_character()),
                          trim_ws = TRUE) %>%
        filter(OTHER_SYSTEM_NAME == "EMS") %>%
        separate(OTHER_SYSTEM_CODE, c("EMS_CODE", "EMS_METHOD_CODE"), 4) %>%
        select(VMV_CODE = vmv_code,
               # VMV_METHOD_CODE = method_code,
               EMS_CODE,
               EMS_METHOD_CODE) %>%
        mutate_all(stringr::str_trim, side = "both") %>%
        mutate_all(stringr::str_squish) %>%
        distinct()

# VMV_CODE should be unique but include VMV_METHOD_CODE for completeness.
# The combination of EMS_CODE & EMS_METHOD_CODE should be unique.
vmv_ems <- left_join(ems_codes, vmv_ems_xwalk,
                     by = c("EMS_CODE",
                            "EMS_METHOD_CODE")) %>%
        full_join(vmv_codes, by = "VMV_CODE") %>%
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
                VMV_METHOD_TITLE,
                PARAM_SHORT_NAME,
                CONSTIT_ABBREV,
                VARIABLE_GROUP,
                PARAM_GROUP,
        )

use_data(ems_codes, vmv_ems, vmv_codes, overwrite = TRUE)
