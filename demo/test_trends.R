# Copyright 2015 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

library(dplyr)
library(ggplot2)

options(wqbc.messages = TRUE)

data(fraser)

fraser$Station <-  factor(sub("BC08", "", as.character(fraser$SiteID)))
fraser <- filter(fraser, Variable == "ARSENIC TOTAL")
fraser <- clean_wqdata(fraser, by = "Station")

trends <-  test_trends(fraser, FUN = median)

# add on annual summary data
data <- summarise_for_trends(fraser, FUN = median)
data <- left_join(trends, data)

data %>% ggplot(aes(x = Year, y = Value, color = sen_slope_sig)) +
         geom_point() +
         facet_wrap(~ Station, scales = "free")
