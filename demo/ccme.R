library(tidyr)
library(dplyr)

data(ccme)

spread(select(ccme, Variable, Value, Date), Variable, Value)
calc_wqis(ccme)
