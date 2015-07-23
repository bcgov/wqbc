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

# This script prepares the example (ccme.csv) data for the package.
# The example data in ccme.csv are from the North Saskatchewan River at Devon -
# 1997, and were taken from the Canadian Council of Ministers of Environment
# (CCME) Water Quality Index Users Manual 1.0, available here:
# http://www.ccme.ca/files/Resources/calculators/WQI%20User%27s%20Manual%20%28en%29.pdf.

library(devtools)

ccme <- read.csv("data-raw/ccme.csv", na.strings = c("NA", ""), stringsAsFactors = FALSE)
ccme$Date <- as.Date(ccme$Date)
ccme$Variable <- factor(as.character(ccme$Variable), levels = unique(ccme$Variable))
ccme$Units <- factor(ccme$Units)
devtools::use_data(ccme, overwrite = TRUE, compress = "xz")
