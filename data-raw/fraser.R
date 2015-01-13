library(wqbc)
library(devtools)
library(dplyr)
library(magrittr)

fraser <- read.csv("data-raw/fraser.csv")

fraser %<>% select(
  SiteID = station_no,
  Date = sample_datetime,
  Variable = variable_name,
  Value = value,
  Units = unit_code,
  DetectionLimit = method_detect_limit,
  Site = station_name,
  Lat = latitude,
  Long = longitude
)

is.na(fraser$Value[fraser$Value == -999.999]) <- TRUE
fraser$Date %<>% as.Date

fraser %<>% filter(substitute_units(Units) %in% get_units())
fraser %<>% filter(substitute_variables(Variable) %in% get_variables())

print(sort(unique(fraser$Variable)))

# check for and flip sign of positive longitude values
fraser$Long <- ifelse(fraser$Long > 0, fraser$Long * -1, fraser$Long)

fraser$SiteID %<>% droplevels
fraser$Variable %<>% droplevels
fraser$Units %<>% droplevels
fraser$Site %<>% droplevels

use_data(fraser, pkg = as.package("."), overwrite = TRUE, compress = "xz")
# improve compression
#tools::resaveRdaFiles("data/fraser.rda")
