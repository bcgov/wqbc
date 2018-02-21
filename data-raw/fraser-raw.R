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

## This script pulls the data from the Fraser River Basin Long-term Water Quality Monitoring dataset,
## using the canwqdata package: https://github.com/bcgov/canwqdata
## The data is saved in fraser/fraser.csv

# install_github("bcgov/canwqdata")
library(canwqdata)
library(dplyr)

fraser <- dl_basin("FRASER-LOWER MAINLAND")

fraser <- tidy_ec_data(fraser)

meta <- wq_sites()
fraser <- left_join(fraser,
                    select(meta, Station = SITE_NO, StationName = SITE_NAME,
                           Lat = LATITUDE, Long = LONGITUDE),
                    by = "Station")

write.csv(fraser, "data-raw/fraser/fraser.csv", row.names = FALSE)
