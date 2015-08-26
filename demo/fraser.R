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
library(lubridate)
library(ggplot2)
library(sp)
library(rgdal)

options(wqbc.messages = TRUE)

data(fraser)
print(summary(fraser))

fraser$SiteID <-  factor(sub("BC08", "", as.character(fraser$SiteID)))
fraser$Year <- year(fraser$Date)
plot_map(fraser, fill = "SiteID")
fraser <- calc_wqi(fraser, by = c("SiteID", "Lat", "Long"))
plot_map_wqis(fraser, shape = "SiteID")

data(fraser)
fraser$Year <- year(fraser$Date)
fraser <- standardize_wqdata(fraser, strict = FALSE)
fraser <- clean_wqdata(fraser, by = "Year", max_cv = Inf)
fraser <- calc_limits(fraser, by = "Year", term = "short")
fraser <- calc_wqi(fraser, by = "Year")
plot_wqis(fraser, x = "Year")
