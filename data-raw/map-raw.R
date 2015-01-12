###############################################################################
## Get map of BC from Govt of Canada Open Data site:
## (http://open.canada.ca/data/en/dataset/f77c2027-ed4a-5f6e-9395-067af3e9fc1e)

map_zip <- "data-raw/map.zip"
map_dir <- "data-raw/map"

## Download the zipfile and unzip:
download.file("http://ftp2.cits.rncan.gc.ca/pub/geott/atlas/base/7.5m_g_shp.zip", destfile = map_zip)

unzip(map_zip, exdir = map_dir)
