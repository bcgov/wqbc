require(devtools)
require(dplyr)
require(magrittr)

waterq <- read.csv("data-raw/waterq.csv")

waterq %<>% select(
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

is.na(waterq$Value[waterq$Value == -999.999]) <- TRUE
waterq$Date %<>% as.Date

levels(waterq$Code) <- list(
  pH = "PH",
  As = "ARSENIC TOTAL",
  Pb = "LEAD TOTAL",
  Ca = "CADMIUM TOTAL")

levels(waterq$Units) <- list(
  "ug/L" = "UG/L",
  "mg/L" = "MG/L",
  "pH" = "PH UNITS")

waterq %<>% filter(!is.na(Code) & !is.na(Value) & !is.na(Units))

use_data(waterq, pkg = as.package("."), overwrite = TRUE, compress = "xz")
# improve compression
#tools::resaveRdaFiles("data/waterq.rda")
