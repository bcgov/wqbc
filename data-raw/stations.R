library(rems)
library(dplyr)
library(magrittr)
library(devtools)

try(download_historic_data())
ems_historic <- read_historic_data()
ems_current <- get_ems_data()

stations <- bind_rows(ems_current, ems_historic)

stations %<>% select(EMS_ID, Station_Name = MONITORING_LOCATION, Latitude = LATITUDE,
                      Longitude = LONGITUDE) %>%
  unique() %>% arrange(EMS_ID) %>% filter(!is.na(Latitude))

stations %<>% filter(!duplicated(EMS_ID))

stations %<>% as.tbl()
use_data(stations, overwrite = TRUE, compress = "xz")
