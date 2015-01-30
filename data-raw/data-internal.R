rm(list = ls())
load("data-raw/codes.rda")

load("data-raw/limits.rda")
load("data-raw/map.rda")

use_data(codes, limits, map, pkg = as.package("."), internal = TRUE,
         overwrite = TRUE, compress = "xz")
