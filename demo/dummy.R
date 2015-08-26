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

library(dplyr)

options(wqbc.messages = TRUE)

data(dummy)
print(dummy)

dummy$Units <- substitute_units(dummy$Units)
dummy$Variable <- substitute_variables(dummy$Variable)
print(dummy)

dummy <- filter(dummy, Units %in% lookup_units() & Variable %in% lookup_variables())
print(dummy)

dummy <- standardize_wqdata(dummy)
print(dummy)

dummy <- clean_wqdata(dummy)
print(dummy)

dummy <- calc_limits(dummy, term = "short")
print(dummy)
