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
fraser <- calc_wqi(fraser, by = c("SiteID", "Lat", "Long"))
plot_map_wqis(fraser, shape = "SiteID")

data(fraser)
fraser$Year <- year(fraser$Date)
fraser <- standardize_wqdata(fraser, strict = FALSE)
fraser <- clean_wqdata(fraser, by = "Year", max_cv = Inf)
fraser <- calc_limits(fraser, by = "Year", term = "short")
fraser <- calc_wqi(fraser, by = "Year")
plot_wqis(fraser, x = "Year")
