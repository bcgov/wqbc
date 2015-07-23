# Copyright 2015 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

# Process the map of BC for inclusion in the package. Map data is in
# data-raw/maps and is downloaded from the Government of Canada Open Data Portal:
# http://open.canada.ca/data/en/dataset/f77c2027-ed4a-5f6e-9395-067af3e9fc1e
# Using map-raw.R

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
