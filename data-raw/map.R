## And import into R as a SpatialPolygonsDataFrame

#install.packages("rgdal", type = "source")

library("rgdal")
library("ggplot2")
library("devtools")
library("dplyr")
library("magrittr")

map_dir <- "data-raw/map"

## Import the "pvp" shapefile, which has provinical boundaries
map <- readOGR(map_dir, "pvp", stringsAsFactors = FALSE)

plot(map)

## Subset to get only BC
map <- map[map$NAME_E == "British Columbia" & !is.na(map$NAME_E),]

## Set the coordinate system to NAD 83 (CSRS) as it is unprojected:
## http://epsg.io/4617
proj4string(map) <- CRS("+init=epsg:4617")

## Project to BC Albers (Standard in BC; an equal-area projection:
## http://epsg.io/3005)
map <- spTransform(map, CRS("+init=epsg:3005"))

plot(map)

## Plot with ggplot2
map <- fortify(map)

map %<>% select(Long = long, Lat = lat, Group = group)

ggplot(map, aes(x = Long, y = Lat, group = Group)) +
  geom_polygon(fill = "#377eb8", size = 0.5, colour = "grey50") +
  coord_fixed() +
  theme_minimal() +
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), panel.grid = element_blank())

save(map, file = "data-raw/map.rda")
