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

context("codes")

test_that("expand_ems_codes", {
  expect_identical(
    expand_ems_codes(c("0014", "KR-P", "0_15", "EMS_ZN_T")),
    c("EMS_0014", "EMS_KR_P", "EMS_0_15", "EMS_ZN_T")
  )
})

test_that("compress_ems_codes", {
  expect_identical(
    compress_ems_codes(c("EMS_0014", "EMS_KR-P", "0-15")),
    c("0014", "KR-P", "0-15")
  )
})
