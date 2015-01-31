library(dplyr)

options(wqbc.messages = TRUE)

data(dummy)
print(dummy)

dummy <- standardize_wqdata(dummy)
print(dummy)

dummy <- clean_wqdata(dummy)
print(dummy)

dummy <- calc_limits(dummy, term = "short")
print(dummy)
