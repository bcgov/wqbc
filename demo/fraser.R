library(dplyr)
library(lubridate)
library(ggplot2)
library(sp)
library(rgdal)

data(fraser)

fraser$Variable <- substitute_variables(fraser$Variable)
fraser$Units <- substitute_units(fraser$Units)
fraser <- dplyr::filter(fraser, Variable %in% get_variables() & Units %in% get_units())
fraser$SiteID <-  factor(sub("BC08", "", as.character(fraser$SiteID)))

plot_map(fraser, fill = "SiteID")

fraser_limits <- calc_limits(fraser, by = c("SiteID", "Lat", "Long"))

fraser_limits$Year <- lubridate::year(fraser_limits$Date)
fraser_wqis <- calc_wqis(fraser_limits, by = c("SiteID", "Year", "Lat", "Long"))

plot_wqis(fraser_wqis, x = "Year") + ggplot2::facet_wrap(~SiteID)

plot_map_wqis(filter(fraser_wqis, Year %in% c(1990, 2010)), keep = "Year") +
  ggplot2::facet_wrap(~Year, ncol = 1)
