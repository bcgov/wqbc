library(devtools)
library(dplyr)
library(magrittr)

fraser <- read.csv("data-raw/fraser.csv")

fraser %<>% select(
  SiteID = station_no,
  Date = sample_datetime,
  Code = variable_name,
  Value = value,
  Units = unit_code,
  DetectionLimit = method_detect_limit,
  Site = station_name,
  Latitude = latitude,
  Longitude = longitude
)

is.na(fraser$Value[fraser$Value == -999.999]) <- TRUE
fraser$Date %<>% as.Date

levels(fraser$Code) <- list(
  pH = "PH",
  As = "ARSENIC TOTAL",
  Pb = "LEAD TOTAL",
  Ca = "CADMIUM TOTAL")

levels(fraser$Units) <- list(
  "ug/L" = "UG/L",
  "mg/L" = "MG/L",
  "pH" = "PH UNITS")

fraser %<>% filter(!is.na(Code) & !is.na(Value) & !is.na(Units))

use_data(fraser, pkg = as.package("."), overwrite = TRUE, compress = "xz")
# improve compression
#tools::resaveRdaFiles("data/fraser.rda")
