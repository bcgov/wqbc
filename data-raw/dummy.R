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

## Makes some fake data for testing and examples.

library(devtools)
library(dplyr)
library(magrittr)

dummy <- data.frame(Date = as.Date("2000-01-01"), Variable = "Aluminium Dissolved",
                   Value = c(20, 1, 2), Units = "mg/L")

dummy %<>% rbind(data.frame(Date = as.Date("2000-01-01"), Variable = "pH",
                               Value = c(8,9.5), Units = "PH UNITS"))

dummy %<>% rbind(data.frame(Date = as.Date("2000-01-02"), Variable = "DISSOLVED ALUMINUM",
                               Value = c(1, 2 * 10^3), Units = c("MG/L", "uG/L")))

dummy %<>% rbind(data.frame(Date = as.Date("2000-01-04"), Variable = "Aluminium Dissolved",
                               Value = c(2 * 10^3, 1), Units = c("ug/L", "mg/L")))

dummy %<>% rbind(data.frame(Date = as.Date("2000-01-05"), Variable = "Aluminium Dissolved",
                               Value = c(1, 40, NA), Units = "mg/L"))

dummy %<>% rbind(data.frame(Date = as.Date("2000-01-06"), Variable = "Aluminium Dissolved",
                               Value = c(10, 20, NA), Units = "mg/L"))

dummy %<>% rbind(data.frame(Date = as.Date("1977-05-25"), Variable = "Zinc Total",
                               Value = c(1, 10^3), Units = c("ug/L", "midichlorians")))

dummy %<>% rbind(data.frame(Date = as.Date("1978-12-01"), Variable = "Kryptonite",
                               Value = 1, Units = "ug/L"))

dummy %<>% rbind(data.frame(Date = as.Date("1978-12-01"), Variable = "pH",
                               Value = 7, Units = "PH UNITS"))

use_data(dummy, pkg = as.package("."), overwrite = TRUE, compress = "xz")
