library(tidyr)
library(lubridate)
library(ggplot2)

tidyr::spread(dplyr::select(ccme, Variable, Value, Date), "Variable", "Value")
data(ccme)
calc_wqis(ccme)
