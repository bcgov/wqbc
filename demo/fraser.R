library(dplyr)
library(lubridate)
library(ggplot2)
library(sp)
library(rgdal)

data(fraser)

fraser$SiteID <-  factor(sub("BC08", "", as.character(fraser$SiteID)))
plot_map(fraser, fill = "SiteID")

fraser_limits <- calc_limits(fraser, by = c("SiteID", "Lat", "Long"),
                             messages = TRUE)

fraser_limits$Year <- year(fraser_limits$Date)
fraser_wqis <- calc_wqis(fraser_limits, by = c("SiteID", "Year", "Lat", "Long"))

plot_wqis(fraser_wqis, x = "Year") + facet_wrap(~SiteID)

plot_map_wqis(filter(fraser_wqis, Year %in% c(1990,2010)), keep = "Year") + facet_wrap(~Year, ncol = 1)
