###############################################################################
## Get map of BC from Govt of Canada Open Data site:
## (http://open.canada.ca/data/en/dataset/f77c2027-ed4a-5f6e-9395-067af3e9fc1e)
## And import into R as a SpatialPolygonsDataFrame

#install.packages("rgdal", type = "source")

library("rgdal")
library("ggplot2")
library("devtools")
library("dplyr")
library("magrittr")

map_zip <- "data-raw/map.zip"
map_dir <- "data-raw/map"

## Download the zipfile and unzip:
download.file("http://ftp2.cits.rncan.gc.ca/pub/geott/atlas/base/7.5m_g_shp.zip", destfile = map_zip)

unzip(map_zip, exdir = map_dir)

## Import the "pvp" shapefile, which has provinical boundaries
map <- readOGR(map_dir, "pvp", stringsAsFactors = FALSE)

plot(map)

## Subset to get only BC
map <- map[map$NAME_E == "British Columbia" & !is.na(map$NAME_E),]

## Set the projection as original is unprojected:
proj4string(map) <- CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")

## Change projection to BC Albers (an equal-area projection)
map <- spTransform(map, CRS("+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))

plot(map)

## Plot with ggplot2
map <- fortify(map)

map %<>% select(Longitude = long, Latitude = lat, Group = group)

ggplot(map, aes(x = Longitude, y = Latitude, group = Group)) +
  geom_polygon(fill = "#377eb8", size = 0.5, colour = "grey50") +
  coord_fixed() +
  theme_minimal() +
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), panel.grid = element_blank())

use_data(map, pkg = as.package("."), overwrite = TRUE, compress = "xz")
