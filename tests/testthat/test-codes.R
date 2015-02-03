context("codes")

test_that("expand_ems_codes", {
  expect_identical(expand_ems_codes(c("0014", "KR-P", "0_15", "EMS_ZN_T")),
                   c("EMS_0014", "EMS_KR_P", "EMS_0_15", "EMS_ZN_T"))
})

test_that("compress_ems_codes", {
  expect_identical(compress_ems_codes(c("EMS_0014", "EMS_KR-P", "0-15")),
                   c("0014", "KR-P", "0-15"))
})
