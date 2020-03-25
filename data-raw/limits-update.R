library(dplyr)

load("data/codes.rda")
load("data/limits.rda")

###### ------ create new limits table from new guidelines
limits_new <- bcdata::bcdc_get_data(record = "85d3990a-ec0a-4436-8ebd-150de3ba0747")
limits_new <- dplyr::mutate(limits_new,
                            Condition = dplyr::if_else(Condition == "",
                                                       NA_character_, Condition)) %>%
  dplyr::filter(Use == "Aquatic Life - Freshwater",
                Direction == "Upper Limit",
                Media == "Water",
                !is.na(EMS_Code)) %>%
  dplyr::mutate(Use = "Freshwater Life") %>%
  dplyr::mutate(Term = dplyr::if_else(Days == 30, "Long", "Short")) %>%
  ### remove cases with ConditionNotes
  dplyr::filter(is.na(ConditionNotes)) %>%
  dplyr::mutate(Variable = paste(Variable, Component),
                UpperLimit = Limit) %>%
  dplyr::select(Variable, Use, Term,
                Condition, UpperLimit, Units,
                Statistic, EMS_Code) %>%
  ### remove cases with multiple EMS_Codes
  dplyr::group_by(Variable, Term, Condition) %>%
  arrange(EMS_Code) %>%
  slice(1) %>%
  ungroup()

### remove duplicates
limits_new <- limits_new %>%
  dplyr::group_by(EMS_Code, Use, Term, Condition) %>%
  dplyr::filter(dplyr::n() == 1) %>%
  dplyr::ungroup()

### ensure that no duplicates
expect_true(all(limits_new %>%
                  dplyr::group_by(EMS_Code, Use, Term, Condition) %>%
                  dplyr::mutate(n = dplyr::n()) %>%
                  dplyr::ungroup() %>%
                  dplyr::pull(n) == 1))

# limits <- limits %>%
#   left_join(select(codes, Variable, Code), "Variable")
#
# limits$EMS_Code <- limits$Code

# get all missing limits from old table and then remove them one by one based on discussion here:
# https://github.com/bcgov/wqg_data/issues/83
# tmp <- anti_join(limits, limits_new, c("EMS_Code", "Term"))
#
# ### remove copper total, nitrogen dissolved, nitrogen total, phosphorous dissolved limits
# tmp <- tmp %>%
#   filter(!(EMS_Code %in% c("EMS_P__D", "EMS_1114", "EMS_0114", "EMS_CU_T", "EMS_0104")))
#
# ### Term is incorrect in old table for EMS_AS_T, EMS_CLO3, EMS_MTBE
# tmp <- tmp %>%
#   filter(!(EMS_Code %in% c("EMS_AS_T", "EMS_MTBE", "EMS_CLO3", "EMS_CL03")))
#
# ### remove Chlorine Residual and Phosphorous Total because have ConditionNotes
# tmp <- tmp %>%
#   filter(!(EMS_Code %in% c("EMS_1016", "EMS_P__T")))
#
# ### remove EMS_NAPH as replace by EMS_PA14
# tmp <- tmp %>%
#   filter(!(EMS_Code %in% c("EMS_NAPH")))
#
# ### remove Alkalinity Total as replaced by Alkalinity (CaCO3) Total
# tmp <- tmp %>%
#   filter(!(EMS_Code == "EMS_AK_T"))
#
# ### confirm that all cases have been dealt with
# expect_identical(nrow(tmp), 0L)


### deal with hardness equations (only include Hardness Total when both Hardness Total and Hardnes Dissolved)
modified <- limits_new$Condition[which(stringr::str_detect(limits_new$Condition, "EMS_0107"))] %>%
  stringr::str_split_fixed("\\s?\\|\\s?", 2)
modified <- modified[, 1]
limits_new$Condition[which(stringr::str_detect(limits_new$Condition, "EMS_0107"))] <- modified

### replace Aluminum with Aluminium
limits_new$Variable %<>%
  stringr::str_replace_all("Aluminum", "Aluminium")

### create new codes table and grab EC_Code from old table where possible
ec_codes <- select(codes, Code, EC_Code) %>%
  filter(Code %in% unique(limits$EMS_Code), !is.na(EC_Code))

codes_new <- limits_new %>%
  select(Variable, Code = EMS_Code, Units) %>%
  distinct() %>%
  left_join(ec_codes, "Code")

missing_codes <- anti_join(codes, codes_new, "Code")
missing_codes$Average <- NULL

codes <- rbind(codes_new, missing_codes)
# remove ems_code error
codes <- codes[!(codes$Code == "EMS_CL03"),]

limits <- limits_new

#### check limits
stopifnot(all(!is.na(select(limits, -Condition))))

stopifnot(all(limits$Term %in% c("Short", "Long")))
stopifnot(all(limits$Units %in% lookup_units()))
stopifnot(all(limits$Use %in% c("Freshwater Life")))

check_valid_expression <- function(x) {
  parse(text = x)
  TRUE
}

check_valid_expression(limits$Condition)
check_valid_expression(limits$UpperLimit)

limits <- rename_(limits, "..Units" = "Units")

stopifnot(all(limits$Variable %in% codes$Variable))

limits <- inner_join(limits, select(codes, EMS_Code = Code, Units), by = "EMS_Code")

stopifnot(all(limits$..Units == limits$Units))
limits$..Units <- NULL

limits  <- limits %>%
  arrange(Variable, Use, Term)
codes <- codes %>%
  arrange(Variable, Code)

limits  <- limits %>%
  select(Variable, Use, Term, Condition, UpperLimit, Units, Statistic)

stopifnot(identical(colnames(codes), c("Variable", "Code", "Units", "EC_Code")))

stopifnot(all(!is.na(codes[c("Variable", "Code", "Units")])))

stopifnot(!anyDuplicated(codes$Code))
stopifnot(!anyDuplicated(codes$Variable))
stopifnot(all(codes$Units %in% lookup_units()))
stopifnot(all(limits$Statistic %in% c("mean", "median", "max")))

use_data(limits, overwrite = TRUE, compress = "xz")
use_data(codes, overwrite = TRUE, compress = "xz")
