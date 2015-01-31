library(dplyr)

msgs <- TRUE

data(dummy)
print(dummy)

dummy_standardized <- standardize_wqdata(dummy, messages = msgs)
print(dummy_standardized)
print(filter(dummy, !ID %in% dummy_standardized$ID))

dummy_cleansed <- clean_wqdata(dummy_standardized, messages = msgs)
print(dummy_cleansed)

dummy_limits <- calc_limits(dummy_cleansed, term = "short", messages = msgs)
print(dummy_limits)
