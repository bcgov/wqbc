library(tidyr)
library(dplyr)

data(ccme)

tidyr::spread(dplyr::select(ccme, Variable, Value, Date), "Variable", "Value")
calc_wqis(ccme, ci = TRUE)
