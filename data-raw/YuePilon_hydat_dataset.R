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

## This script downloads the full HYDAT Database, a National Water Data Archive,
## available at: http://www.ec.gc.ca/rhc-wsc/default.asp?lang=En&n=9018B5EC-1,
## and extracts only the data used in the Yue Pilon paper cited in the zyp packages
## under the Open Government License - Canada version 2.0 (http://open.canada.ca/en/open-government-licence-canada)
## The data is saved in hydat/Hydat.sqlite3

# download data -----------------------

# download hydat database 'Hydat_sqlite3_20161017.zip' from:
# http://collaboration.cmc.ec.gc.ca/cmc/hydrometrics/www/
hydatzip <- "Hydat_sqlite3_20161017.zip"
if (!file.exists(paste0("data-raw/hydat/", hydatzip))) {
  ret <-
    download.file(
      url = paste0("http://collaboration.cmc.ec.gc.ca/cmc/hydrometrics/www/", hydatzip),
      destfile = paste0("data-raw/hydat/", hydatzip), mode = "wb"
    )
  if (ret != 0) stop("Error downloading hydata database")
}

# unzip
if (!file.exists(paste0("data-raw/hydat/Hydat.sqlite3"))) {
  unzip(paste0("data-raw/hydat/", hydatzip), exdir = "data-raw/hydat")
}
rm(hydatzip)


# connect to database and filter -----------------
# requires DBI and RSQLite to be installed

# connect HYDAT database
hydat <- DBI::dbConnect(RSQLite::SQLite(), "data-raw/hydat/Hydat.sqlite3")

# inspect contents
if (FALSE) {
  DBI::dbListTables(hydat)
  DBI::dbListFields(hydat, "DLY_FLOWS")
  DBI::dbListFields(hydat, "STATIONS")
}

# Fetch all columns from daily flows data for the 4 stations used in Yue Pilon
res <- DBI::dbSendQuery(hydat, 'SELECT * FROM DLY_FLOWS
                                WHERE STATION_NUMBER IN ("02FB007", "02KB001", "02EA005", "02GA010")
                                      AND
                                      YEAR BETWEEN 1949 and 1999')
yuepilon <- DBI::dbFetch(res)
DBI::dbClearResult(res)

# get station info table
res <- DBI::dbSendQuery(hydat, 'SELECT * FROM STATIONS
                                WHERE STATION_NUMBER IN ("02FB007", "02KB001", "02EA005", "02GA010")')
stations <- DBI::dbFetch(res)
DBI::dbClearResult(res)

DBI::dbDisconnect(hydat)
rm(hydat, res)


# process data into appropriate format for wqbc -----------
library(tidyr)
library(dplyr)

# summarise
if (FALSE) {
  table(yuepilon$STATION_NUMBER, yuepilon$YEAR)
}

# column names to lower case
names(yuepilon) <- tolower(names(yuepilon))

# select only data columns
yuepilon <-
  yuepilon %>%
  select(station_number:month, matches("^flow[0-9]+$")) %>%
  as_tibble()

# restructure and filter out NA obs
yuepilon <-
  yuepilon %>%
  gather(day, flow, -(1:3)) %>%
  mutate(day = as.integer(sub("flow", "", day))) %>%
  filter(!is.na(flow))

# calculate annual means
yuepilon <-
  yuepilon %>%
  select(station_number, year, flow) %>%
  group_by(station_number, year) %>%
  summarise(flow = mean(flow)) %>%
  ungroup() %>%
  mutate(Date = as.Date(as.character(year), format = "%Y"))

# rename colmns
yuepilon <-
  yuepilon %>%
  rename(Station = station_number, Value = flow) %>%
  mutate(Variable = "mean_annual_flow", Units = "m^3/s") %>%
  select(Station, Date, Variable, Value, Units)

# join on station info
stations <-
  stations %>%
  rename(Station = STATION_NUMBER, Site = STATION_NAME, Lat = LATITUDE, Long = LONGITUDE) %>%
  select(Station, Site, Lat, Long)

yuepilon <-
  yuepilon %>%
  left_join(stations, by = "Station")

# save data for use in package ---------------
library(devtools)

# save data as csv
write.csv(yuepilon, "data-raw/hydat/yuepilon.csv", row.names = FALSE)

# add data to package
use_data(yuepilon, pkg = as.package("."), overwrite = TRUE, compress = "xz")
# improve compression
# tools::resaveRdaFiles("data/yuepilon.rda")
