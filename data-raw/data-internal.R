rm(list = ls())

load("data-raw/map.rda")

use_data(map, pkg = as.package("."), internal = TRUE,
         overwrite = TRUE, compress = "xz")
