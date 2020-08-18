library(dplyr)
library(chk)

###### ------ create new limits table from new guidelines
limits <- bcdata::bcdc_get_data(record = "85d3990a-ec0a-4436-8ebd-150de3ba0747")

codes <- read.csv("data-raw/codes.csv", na.strings = c("NA", ""), stringsAsFactors = FALSE)

check_key(codes, "Code")

ec_codes <- read.csv("data-raw/ec-codes.csv", na.strings = c("NA", ""), stringsAsFactors = FALSE)

check_key(ec_codes, "EMS_Code")
check_key(ec_codes, "EC_Code")

limits <- dplyr::mutate(limits,
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
  ungroup() %>%
  ### remove duplicates
  dplyr::group_by(EMS_Code, Use, Term, Condition) %>%
  dplyr::filter(dplyr::n() == 1) %>%
  dplyr::ungroup()

### deal with hardness equations (only include Hardness Total when both Hardness Total and Hardness Dissolved)
modified <-
  limits$Condition[which(stringr::str_detect(limits$Condition, "EMS_0107"))] %>%
  stringr::str_split_fixed("\\|", 2)
modified <- modified[stringr::str_detect(modified, "EMS_0107")]
limits$Condition[which(stringr::str_detect(limits$Condition, "EMS_0107"))] <- modified

codes_limits <- limits %>%
  select(Variable, Code = EMS_Code, Units) %>%
  distinct()

# codes that already in limits
semi_join(codes, codes_limits, "Code")

codes <- anti_join(codes, codes_limits, "Code")

codes %<>% rbind(codes_limits)  %>%
  left_join(ec_codes, by = c(Code = "EMS_Code"))

# remove ems_code error
codes <- codes[!(codes$Code == "EMS_CL03"),]

limits <- rename(limits, "..Units" = "Units")

stopifnot(all(limits$Variable %in% codes$Variable))

limits <- inner_join(limits, select(codes, EMS_Code = Code, Units), by = "EMS_Code")

stopifnot(all(limits$..Units == limits$Units))

limits  <- limits %>%
  select(Variable, Use, Term, Condition, UpperLimit, Units, Statistic) %>%
  arrange(Variable, Use, Term)

codes <- codes %>%
  arrange(Variable, Code)

poisdata::ps_duplicates(codes, "Code")

check_codes(codes)
check_limits(limits, codes = FALSE)

use_data(limits, overwrite = TRUE, compress = "xz")
use_data(codes, overwrite = TRUE, compress = "xz")
