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
  Latitude = latitude,
  Longitude = longitude
)

is.na(fraser$Value[fraser$Value == -999.999]) <- TRUE
fraser$Date %<>% as.Date

fraser %<>% filter(Units %in% c("UG/L", "MG/L", "CFU/100ML", "PH UNITS", "NTU"))

print(sort(unique(fraser$Variable)))

fraser %<>% filter(Variable %in% c("ALUMINUN DISSOLVED", "ALUMINUM TOTAL",
                                   "ARSENIC TOTAL", "LEAD TOTAL",
                                   "HARDNESS TOTAL CACO3", "PH",
                                   "OXYGEN DISSOLVED", "NITROGEN TOTAL",
                                   "ESCHERICHIA COLI",
                                   "ENTEROCOCUS",
                                   "HARDNESS TOTAL (CALCD.) CACO3",
                                   "HARDNESS TOTAL CACO3"))

# check for and flip sign of positive longitude values
fraser$Longitude <- ifelse(fraser$Longitude > 0, fraser$Longitude * -1, fraser$Longitude)

fraser$SiteID %<>% droplevels
fraser$Variable %<>% droplevels
fraser$Units %<>% droplevels
fraser$Site %<>% droplevels

use_data(fraser, pkg = as.package("."), overwrite = TRUE, compress = "xz")
# improve compression
#tools::resaveRdaFiles("data/fraser.rda")
