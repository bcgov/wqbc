library(tidyr)
library(dplyr)

options(wqbc.messages = TRUE)

data(ccme)

spread(select(ccme, Variable, Value, Date), Variable, Value)

calc_wqis(ccme)
