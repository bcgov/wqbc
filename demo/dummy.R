library(dplyr)

data(dummy)

print(dummy)
dummy_standardized <- standardize_wqdata(dummy, messages = TRUE)
print(dummy_standardized)
print(filter(dummy, !ID %in% dummy_standardized$ID))

dummy_cleansed <- clean_wqdata(dummy_standardized, messages = TRUE)
print(dummy_cleansed)

dummy_limits <- calc_limits(dummy_cleansed, messages = TRUE)
