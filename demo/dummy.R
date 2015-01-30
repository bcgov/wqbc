library(dplyr)
library(magrittr)

# data(dummy)
# print(dummy)
# dummy <- clean_wqdata(dummy)
# print(dummy)
# dummy <- calc_limits(dummy)
# print(dummy)
# calc_wqis(dummy)


data(dummy)
print(dummy)
dummy %<>% filter(!is.na(Date) & !is.na(Variable) & !is.na(Value) & !is.na(Units))
print(dummy)
dummy$Variable %<>% substitute_variables
dummy$Units %<>% substitute_units
print(dummy)
dummy %<>% filter(Variable %in% get_variables() & Units %in% get_units())
print(dummy)
dummy %<>% calc_rcvs
print(dummy)
dummy %<>% filter(CV < 1)
print(dummy)
dummy %<>% standardise_values_units
print(dummy)
