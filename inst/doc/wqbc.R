## ---- echo = FALSE, message = FALSE--------------------------------------
library(wqbc)
library(lubridate)
library(xtable)
library(tidyr)
library(dplyr)
library(ggplot2)

options(wqbc.messages = TRUE)

knitr::opts_chunk$set(
  comment = "#>",
  error = FALSE,
  tidy = FALSE)

## ---- eval = FALSE-------------------------------------------------------
#  library(wqbc)

## ----ccmedata, echo=FALSE, results='asis'--------------------------------
data(ccme)
tab <- spread(select(ccme, Variable, Value, Date), Variable, Value)
tab $ Date <- as.character(tab $ Date)
# Analysis of variance.
print(xtable(tab), type = "html")

## ------------------------------------------------------------------------
data(ccme)

## ------------------------------------------------------------------------
head(ccme, 12)

## ---- echo = 2-----------------------------------------------------------
options(wqbc.messages = FALSE)
calc_wqi(ccme)
options(wqbc.messages = TRUE)

## ---- echo = FALSE-------------------------------------------------------
head(ccme[c("Variable", "Value", "Units")], 5)

## ------------------------------------------------------------------------
lookup_limits()

## ------------------------------------------------------------------------
head(ccme, 5)

## ------------------------------------------------------------------------
ccme2 <- ccme
ccme2 $ UpperLimit <- 50
calc_wqi(ccme2)

## ------------------------------------------------------------------------
data(fraser)

## ------------------------------------------------------------------------
head(fraser, 10)

## ---- fig.width = 7, fig.height = 7, dpi = 100---------------------------
plot_map(fraser, fill = "SiteID")

## ---- eval = FALSE-------------------------------------------------------
#  calc_wqi(fraser, by = "SiteID")

## ------------------------------------------------------------------------
library(lubridate)
data2012 <- subset(fraser, year(Date) == 2012)
head(data2012)

## ---- eval = FALSE-------------------------------------------------------
#  calc_wqi(data2012)

## ------------------------------------------------------------------------
lookup_limits(ph = 7)

## ------------------------------------------------------------------------
data2012 <- standardize_wqdata(data2012)
head(data2012)

## ---- eval = FALSE-------------------------------------------------------
#  clean_wqdata(data2012)

## ------------------------------------------------------------------------
data2012 <- clean_wqdata(data2012, by = "SiteID")

## ------------------------------------------------------------------------
calc_limits(data2012, by = "SiteID", term = "long")

## ---- fig.width = 7, fig.height = 7, dpi = 100---------------------------
qplot(Date, SiteID, xlab = "", ylab = "", data = data2012, colour = SiteID == "BC08MC0001")

## ------------------------------------------------------------------------
data2012 <- calc_limits(data2012, by = "SiteID", term = "short") 
head(data2012, 12)

## ---- eval = FALSE-------------------------------------------------------
#  data2012 <- subset(fraser, year(Date) == 2012)
#  data2012 <- standardize_wqdata(data2012)
#  data2012 <- clean_wqdata(data2012, by = "SiteID")
#  data2012 <- calc_limits(data2012, by = "SiteID", term = "short")

## ------------------------------------------------------------------------
wqi2012 <- calc_wqi(data2012, by = "SiteID") 
wqi2012

## ---- echo = c(2:3)------------------------------------------------------
options(wqbc.messages = FALSE)
data2012 <- subset(fraser, year(Date) == 2012)
calc_wqi(data2012) 

## ---- message = FALSE----------------------------------------------------
options(wqbc.messages = FALSE)
data2012 <- subset(fraser, year(Date) == 2012)
data2012 <- calc_limits(data2012, by = c("SiteID", "Lat", "Long"), term = "short") 
head(data2012)

## ---- message = FALSE----------------------------------------------------
wqi2012 <- calc_wqi(data2012, by = c("SiteID", "Lat", "Long")) 
wqi2012

## ---- fig.width = 7, fig.height = 7, dpi = 100---------------------------
plot_map(wqi2012, fill = "WQI")

## ---- fig.width = 7, fig.height = 7, dpi = 100---------------------------
plot_map_wqis(wqi2012)

## ------------------------------------------------------------------------
options(wqbc.messages = FALSE)
dataNorthSouth <- subset(fraser, year(Date) %in% 2012)
dataNorthSouth $ NorthSouth <- ifelse(dataNorthSouth $ Lat < 52, "South", "North")
limitsNorthSouth <- calc_limits(dataNorthSouth, by = "NorthSouth", term = "short") 
wqiNorthSouth <- calc_wqi(limitsNorthSouth, by = "NorthSouth") 

## ---- fig.width = 7, fig.height = 7, dpi = 100---------------------------
wqiNorthSouth <- merge(unique(dataNorthSouth[c("NorthSouth", "SiteID", "Lat", "Long")]), wqiNorthSouth)
wqiNorthSouth
plot_map(wqiNorthSouth, fill = "WQI")

## ------------------------------------------------------------------------
options(wqbc.messages = TRUE)
data07to12 <- subset(fraser, year(Date) %in% 2007:2012)
data07to12 $ year <- year(data07to12 $ Date)
limits07to12 <- calc_limits(data07to12, by = c("year", "SiteID", "Lat", "Long"), term = "short") 
wqi07to12 <- calc_wqi(limits07to12, by = c("year", "SiteID", "Lat", "Long")) 

## ---- fig.width = 7, fig.height = 5, dpi = 100---------------------------
p <- plot_map_wqis(wqi07to12, keep = "year") 
p + facet_wrap(~year)

## ------------------------------------------------------------------------
library(tidyr)
library(dplyr)

options(wqbc.messages = TRUE)

data(ccme)

spread(select(ccme, Variable, Value, Date), Variable, Value)

calc_wqi(ccme)

## ---- eval = FALSE, fig.width = 7, fig.height = 7, dpi = 100-------------
#  library(dplyr)
#  library(lubridate)
#  library(ggplot2)
#  library(sp)
#  library(rgdal)
#  
#  options(wqbc.messages = TRUE)
#  
#  data(fraser)
#  print(summary(fraser))
#  
#  fraser$SiteID <-  factor(sub("BC08", "", as.character(fraser$SiteID)))
#  fraser$Year <- year(fraser$Date)
#  plot_map(fraser, fill = "SiteID")
#  fraser <- calc_wqi(fraser, by = c("SiteID", "Lat", "Long"))
#  plot_map_wqis(fraser, shape = "SiteID")
#  
#  data(fraser)
#  fraser$Year <- year(fraser$Date)
#  fraser <- standardize_wqdata(fraser, strict = FALSE)
#  fraser <- clean_wqdata(fraser, by = "Year", max_cv = Inf)
#  fraser <- calc_limits(fraser, by = "Year", term = "short")
#  fraser <- calc_wqi(fraser, by = "Year")
#  plot_wqis(fraser, x = "Year")

