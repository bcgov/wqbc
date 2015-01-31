library(dplyr)
library(lubridate)
library(ggplot2)
library(sp)
library(rgdal)

options(wqbc.messages = TRUE)

data(fraser)
print(summary(fraser))

fraser$SiteID <-  factor(sub("BC08", "", as.character(fraser$SiteID)))
fraser$Year <- year(fraser$Date)

plot_map(fraser, fill = "SiteID")

fraser <- calc_wqis(fraser, by = c("SiteID", "Year", "Lat", "Long"))

plot_wqis(fraser, x = "Year") + facet_wrap(~SiteID)
plot_map_wqis(filter(fraser, Year %in% c(2000,2010)), keep = "Year") +
  facet_wrap(~Year, ncol = 1)

# Short-term WQIs by year
data(fraser)
fraser$Year <- year(fraser$Date)
fraser <- standardize_wqdata(fraser, strict = FALSE)
fraser <- clean_wqdata(fraser, by = "Year", max_cv = Inf)
fraser <- calc_limits(fraser, by = "Year", term = "short")
fraser <- calc_wqis(fraser, by = "Year")
plot_wqis(fraser, x = "Year")
