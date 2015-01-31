library(dplyr)
library(lubridate)
library(ggplot2)
library(sp)
library(rgdal)

msgs <- TRUE

data(fraser)
print(summary(fraser))

fraser$SiteID <-  factor(sub("BC08", "", as.character(fraser$SiteID)))
plot_map(fraser, fill = "SiteID")

fraser_standard <- standardize_wqdata(fraser, messages = msgs)
fraser_clean <- clean_wqdata(fraser_standard, by = c("SiteID", "Lat", "Long"), messages = msgs)

for(term in c("short", "long")) {
  print(paste0(term, "-term"))

  fraser_limits <- calc_limits(fraser_clean, by = c("SiteID", "Lat", "Long"),
                               term = term, messages = msgs)

  fraser_limits$Year <- year(fraser_limits$Date)
  fraser_limits <- calc_wqis(fraser_limits, by = c("SiteID", "Year", "Lat", "Long"))

  print(plot_wqis(fraser_limits, x = "Year") + facet_wrap(~SiteID))

  print(plot_map_wqis(filter(fraser_limits, Year %in% c(2000,2010)), keep = "Year") + facet_wrap(~Year, ncol = 1))
}
