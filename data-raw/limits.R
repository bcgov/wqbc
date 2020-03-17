# Copyright 2015 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

## Process the wq limits (guidelines) in limits.csv for inclusion in the
## package. See limits.Rmd for details

library(wqbc)
library(dplyr)
library(magrittr)
library(devtools)

rm(list = ls())

input_limits <- function() {
  limits <- read.csv("data-raw/limits.csv", na.strings = c("NA", ""), stringsAsFactors = FALSE)

  stopifnot(identical(
    colnames(limits),
    c(
      "Variable", "Term",
      "Condition", "UpperLimit", "Units", "Table",
      "Reference", "Use"
    )
  ))

  stopifnot(all(!is.na(select(limits, -Condition))))

  stopifnot(all(limits$Term %in% c("Short", "Long")))
  stopifnot(all(limits$Units %in% lookup_units()))
  stopifnot(all(limits$Reference %in% c(
    "BC_2006", "BC_2015", "MOE_PERS_COMM_2014",
    "MOE_PERS_COMM_2015", "CANADA_2014"
  )))
  stopifnot(all(limits$Use %in% c("Freshwater Life")))

  check_valid_expression <- function(x) {
    parse(text = x)
    TRUE
  }

  check_valid_expression(limits$Condition)
  check_valid_expression(limits$UpperLimit)

  limits <- rename_(limits, "..Units" = "Units")

  load("data/codes.rda")

  stopifnot(all(limits$Variable %in% codes$Variable))

  limits <- inner_join(limits, codes, by = "Variable")

  stopifnot(all(limits$..Units == limits$Units))
  limits$..Units <- NULL

  limits %<>% arrange(Variable, Use, Term)

  limits %<>% select(Variable, Use, Term, Condition, UpperLimit, Units)

  limits
}
limits <- input_limits()

input_codes <- function() {
  codes <- read.csv("data-raw/codes.csv", na.strings = c("NA", ""), stringsAsFactors = FALSE)
  stopifnot(identical(colnames(codes), c("Variable", "Code", "Units", "Average", "EC_Code")))

  stopifnot(all(!is.na(codes[c("Variable", "Code", "Units", "Average")])))

  # codes <- rems::ems_parameters %>%
  #   select(Variable = PARAMETER, Code = PARAMETER_CODE, Units = UNIT) %>%
  #   mutate(Code = wqbc:::expand_ems_codes(Code)) %>%
  #   left_join(select(codes, -Variable, -EC_Code), by = c("Code", "Units")) %>%
  #   distinct()
  #
  # codes$Average[is.na(codes$Average)] <- "mean"

  stopifnot(!anyDuplicated(codes$Code))
  stopifnot(!anyDuplicated(codes$Variable))
  stopifnot(all(codes$Units %in% lookup_units()))
  stopifnot(all(codes$Average %in% c("mean", "median")))

  codes
}
codes <- input_codes()

limits2 <- readr::read_csv("~/Poisson/Data/shinywqg/all_wqgs.csv")
# limits2 <- bcdata::bcdc_get_data(record = "85d3990a-ec0a-4436-8ebd-150de3ba0747")
limits_new <- dplyr::mutate(limits2,
                            Condition = dplyr::if_else(Condition == "",
                                                       NA_character_, Condition)) %>%
  dplyr::filter(Use == "Aquatic Life - Freshwater",
                Direction == "Upper Limit",
                Media == "Water") %>%
  dplyr::mutate(Use = "Freshwater Life") %>%
  dplyr::mutate(Term = dplyr::if_else(Days == 30, "Long", "Short")) %>%
  ### cases with multiple EMS_CODE
  # dplyr::group_by(Variable) %>%
  # dplyr::mutate(EMS_Code2 = dplyr::first(EMS_Code),
  #               EMS_Code3 = paste(unique(EMS_Code), collapse = "; ")) %>%
  # dplyr::ungroup() %>%
  # dplyr::filter(EMS_Code2 == EMS_Code) %>%
  ### remove cases with ConditionNotes
  dplyr::filter(is.na(ConditionNotes)) %>%
  dplyr::mutate(Variable = paste(Variable, Component),
                UpperLimit = Limit) %>%
  dplyr::select(Variable, Use, Term,
                Condition, UpperLimit, Units,
                Statistic, EMS_Code)

### remove mercury duplicate if hasnt been resolved yet on bcdata
mercury_index <- limits_new$Variable == "Mercury Total" & limits_new$UpperLimit == 0.02

if(is.na(limits_new$Condition[mercury_index])){
  limits_new <- limits_new[!mercury_index,]
}

### ensure that no duplicates
expect_true(all(limits_new %>%
                  dplyr::group_by(Variable, Use, Term, Condition) %>%
                  dplyr::mutate(n = dplyr::n()) %>%
                  dplyr::ungroup() %>%
                  dplyr::pull(n)))

codes_new <- limits_new %>%
  select(Variable, Code = EMS_Code, Units, Average = Statistic) %>%
  mutate(EC_Code = NA_integer_)

missing_codes <- anti_join(codes_new, codes,
                           by = c("Variable", "Code", "Units", "Average"))
codes <- rbind(missing_codes, codes) %>%
  distinct()

limits_new$Statistic <- NULL

limits <- limits %>%
  left_join(select(codes, Variable, Code), "Variable")

limits$EMS_Code <- limits$Code
limits$Code <- NULL

tmp <- rbind(limits, limits_new) %>%
  group_by(EMS_Code, Term) %>%
  mutate(n = n()) %>%
  filter(n > 1) %>%
  ungroup() %>%
  select(Variable, Term, EMS_Code, Condition,UpperLimit) %>%
  arrange(EMS_Code, Term)

### check for diff in Term
tmp <- limits_new %>%
  select(EMS_Code, Term) %>%
  distinct() %>%
  mutate(Limit = "new")

tmp2 <- limits %>%
  select(EMS_Code, Term) %>%
  distinct() %>%
  mutate(Limit = "old")

tmp3 <- tmp2 %>%
  left_join(tmp, c("EMS_Code", "Term"))

### fluoride total use new limits
### nitrate total use new limits
### nitrite dissolved keep old limits

### --- for now, leave in old limits that have no replacement in new data
### add copper with Media Sediment back
copper_limit <- limits2 %>% filter(EMS_Code == "EMS_CU_T",
                                   Use == "Aquatic Life - Freshwater",
                                   Media == "Sediment",
                                   PredictedEffectLevel == "No Effect") %>%
  mutate(Term = "Short",
         UpperLimit = Limit) %>%
  select(names(limits))
limits_new <- bind_rows(limits_new, copper_limit)

### add copper with Term Long
copper_limit2 <- limits %>% filter(EMS_Code == "EMS_CU_T",
                                   Term == "Long")
limits_new <- bind_rows(limits_new, copper_limit2)

### add Phosphorous Total with ConditionNote back
phos_limit <- limits2 %>% filter(EMS_Code == "EMS_P__T",
                                 Use == "Aquatic Life - Freshwater") %>%
  mutate(Term = "Long", UpperLimit = Limit) %>%
  select(names(limits))
limits_new <- bind_rows(limits_new, phos_limit)

### add limits with different Term
term_limits <- limits %>% filter(EMS_Code %in% c("EMS_AS_T", "EMS_CLO3", "EMS_CL03", "EMS_MTBE")) %>%
  distinct()
limits_new <- bind_rows(limits_new, term_limits)

### add Chlorine Residual Total
chl_limits <- limits %>% filter(EMS_Code == "EMS_1016")
limits_new <- bind_rows(limits_new, chl_limits)

### add Chloride Total (EMS_0104)
chl2_limits <- limits %>% filter(EMS_Code == "EMS_0104")
limits_new <- bind_rows(limits_new, chl2_limits)

### napthalene alternative EMS_Code
napth_limits <- limits_new %>% filter(EMS_Code == "EMS_PA14")
napth_limits$EMS_Code <- "EMS_NAPH"
limits_new <- bind_rows(limits_new, napth_limits)

### add missing variables
missing_limits <- limits %>% filter(EMS_Code %in% c("EMS_1114", "EMS_0114", "EMS_P__D"))
limits_new <- bind_rows(limits_new, missing_limits)

## check that all old EMS_Code and Term combinations accounted for in new limits

tmp <- anti_join(limits, limits_new, c("EMS_Code", "Term"))
expect_identical(nrow(tmp), 0L)

limits <- limits_new

## check that all can be matched to codes
tmp <- anti_join(limits, codes, c("EMS_Code" = "Code"))
expect_identical(nrow(tmp), 0L)

use_data(limits, overwrite = TRUE, compress = "xz")
use_data(codes, overwrite = TRUE, compress = "xz")

