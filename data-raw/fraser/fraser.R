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

## This script processes the Fraser River data (fraser.csv) that was downloaded using
## fraser-raw.R

library(wqbc)
library(devtools)
library(plyr)
library(dplyr)
library(magrittr)

fraser <- read.csv("data-raw/fraser/fraser.csv")

fraser %<>% select(
  SiteID = station_no,
  Date = sample_datetime,
  Variable = variable_name,
  Value = value,
  Units = unit_code,
  Site = station_name,
  Lat = latitude,
  Long = longitude
)

fraser$Date %<>% as.Date

# check for and flip sign of positive longitude values
fraser$Long <- ifelse(fraser$Long > 0, fraser$Long * -1, fraser$Long)

mean_lat_long <- function (x) {
  x$Lat <- mean(x$Lat)
  x$Long <- mean(x$Long)
  x
}

fraser %<>% ddply("Site", mean_lat_long)

fraser$SiteID %<>% droplevels
fraser$Variable %<>% droplevels
fraser$Units %<>% droplevels
fraser$Site %<>% droplevels

use_data(fraser, pkg = as.package("."), overwrite = TRUE, compress = "xz")
# improve compression
#tools::resaveRdaFiles("data/fraser.rda")
