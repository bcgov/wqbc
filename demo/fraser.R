library(magrittr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(sp)
library(rgdal)
library(foreach)
library(doParallel)

if (getDoParWorkers() == 1) {
  registerDoParallel(3)
}

data(fraser)

fraser$Variable %<>% substitute_variables
fraser$Units %<>% substitute_units
fraser %<>% dplyr::filter(Variable %in% get_variables() & Units %in% get_units())
fraser$SiteID %<>% as.character %>% sub("BC08", "", .) %>% factor

plot_map(fraser, fill = "SiteID")

fraser_limits <- calc_limits(fraser, by = c("SiteID", "Lat", "Long"),
                             parallel = TRUE)

fraser_limits$Year <- year(fraser_limits$Date)
fraser_wqis <- calc_wqis(fraser_limits, by = c("SiteID", "Year", "Lat", "Long"),
                         parallel = TRUE)

plot_wqis(fraser_wqis, x = "Year") + facet_wrap(~SiteID)

plot_map_wqis(filter(fraser_wqis, Year %in% c(1990, 2010)), drop = FALSE) +
  facet_wrap(~Year, ncol = 1)

