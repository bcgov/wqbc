library(tidyr)
library(dplyr)

tidyr::spread(dplyr::select(ccme, Variable, Value, Date), "Variable", "Value")
data(ccme)
calc_wqis(ccme)
