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

###############################################################################
## This script pulls the national and provincial base maps from the Govt of Canada Open Data site,
## available at: http://open.canada.ca/data/en/dataset/f77c2027-ed4a-5f6e-9395-067af3e9fc1e
## under the Open Government License - Canada version 2.0 (http://open.canada.ca/en/open-government-licence-canada)
## The data is saved in data-raw/map

map_zip <- "data-raw/map/map.zip"
map_dir <- "data-raw/map"

## Download the zipfile and unzip:
download.file("http://ftp2.cits.rncan.gc.ca/pub/geott/atlas/base/7.5m_g_shp.zip", destfile = map_zip)

unzip(map_zip, exdir = map_dir)
