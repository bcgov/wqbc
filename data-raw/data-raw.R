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

## The overarching script to prepare data for the package

devtools::load_all() # rebuild package
source("data-raw/dummy.R")
source("data-raw/limits-and-codes.R")
source("data-raw/site-limits.R")
devtools::load_all() # rebuild package
# source("data-raw/fraser-raw.R")
source("data-raw/fraser.R")
# source("data-raw/map-raw.R")
source("data-raw/map.R")
source("data-raw/ccme.R")
source("data-raw/data-internal.R")
source("data-raw/ems-vmv-codes.R")
source("data-raw/stations.R")
# source("data-raw/YuePilon_hydat_dataset.R")
