# Copyright 2016 Province of British Columbia
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

context("test-trends")

test_that("test_trends", {
  expect_error(test_trends(wqbc::dummy))
  trend <- test_trends(wqbc::yuepilon)
  expect_identical(nrow(trend), 4L)
  expect_identical(ncol(trend), 8L)
  expect_is(trend$estimate, "numeric")
  expect_is(trend$months, "character")
  expect_lt(abs(trend$estimate[1] - 0.01350760), 1e-6)
})


# test values
# test breaks
# sanity check



