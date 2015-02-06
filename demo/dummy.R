library(dplyr)

options(wqbc.messages = TRUE)

data(dummy)
print(dummy)

dummy$Units <- substitute_units(dummy$Units)
dummy$Variable <- substitute_variables(dummy$Variable)
print(dummy)

dummy <- filter(dummy, Units %in% lookup_units() & Variable %in% lookup_variables())
print(dummy)

dummy <- standardize_wqdata(dummy)
print(dummy)

dummy <- clean_wqdata(dummy)
print(dummy)

dummy <- calc_limits(dummy, term = "short")
print(dummy)
