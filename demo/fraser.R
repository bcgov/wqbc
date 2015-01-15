library(ggplot2)
library(sp)
library(rgdal)

data(fraser)
data <- fraser

plot_map(data)

data$Variable %<>% substitute_variables
data$Units %<>% substitute_units
data %<>% filter(Variable %in% get_variables() & Units %in% get_units())

data %<>% calc_limits()

site <- calc_wqis(data, by = "Site")

data %<>% calc_limits(by = c("Site", "Lat", "Long"))
site <- calc_wqis(data, by = c("Site", "Lat", "Long"))

plot_map(site, by = "Site")

year <- calc_wqis(data, by = c("Year"))
site_year <- calc_wqis(data, by = c("Site", "Year"))

plot_wqis(year, x = "Year")
plot_wqis(site_year, x = "Year") + facet_wrap(~Site)
