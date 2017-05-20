context("tidy data")
library(dplyr)

test_ec <- structure(list(
  SITE_NO = c("BC08NL0001", "BC08NL0001", "BC08NL0001"),
  DATE_TIME_HEURE = c("11/01/2000 9:15", "11/01/2000 9:15", "11/01/2000 9:15"),
  FLAG_MARQUEUR = c(NA, NA, "<"),
  VALUE_VALEUR = c(19.6, NA, 0.002),
  SDL_LDE = c(0.01, 2e-04, 0.002),
  MDL_LDM = c(0.01, 2e-04, 0.002),
  VMV_CODE = c(100493L, 100217L, 100474L),
  UNIT_UNITÉ = c("UG/L", "MG/L", "UG/L"),
  VARIABLE = c("BARIUM EXTRACTABLE", "BARIUM TOTAL", "BERYLLIUM EXTRACTABLE"),
  VARIABLE_FR = c("BARYUM EXTRACTIBLE", "BARYUM TOTAL", "BÉRYLLIUM EXTRACTIBL"),
  STATUS_STATUT = c("P", "P", "P")),
  class = "data.frame",
  .Names = c("SITE_NO", "DATE_TIME_HEURE", "FLAG_MARQUEUR",
             "VALUE_VALEUR", "SDL_LDE", "MDL_LDM", "VMV_CODE","UNIT_UNITÉ", "VARIABLE",
             "VARIABLE_FR", "STATUS_STATUT"), row.names = c(NA, -3L))

test_ems <- select(test_ec,
                   EMS_ID = SITE_NO,
                   COLLECTION_START = DATE_TIME_HEURE,
                   RESULT_LETTER = FLAG_MARQUEUR,
                   PARAMETER = VARIABLE,
                   RESULT = VALUE_VALEUR,
                   UNIT = UNIT_UNITÉ,
                   METHOD_DETECTION_LIMIT = SDL_LDE) %>%
  mutate(MONITORING_LOCATION = letters[1:3],
         PARAMETER_CODE = LETTERS[1:3],
         COLLECTION_START = as.POSIXct(COLLECTION_START, format = "%d/%m/%Y %k"))

test_that("set_non_detects works with zeros", {
  expect_equal(set_non_detects(c(1.5, 2.0, 2.5), mdl_flag = c("<", NA, NA), mdl_action = "zero"),
               c(0, 2.0, 2.5))
  expect_equal(set_non_detects(c(1.5, 2.0, 2.5), mdl_value = c(1.9, 1.9, 1.9),
                       mdl_action = "zero"),
               c(0, 2.0, 2.5))
  expect_equal(set_non_detects(c(1.5, 2.0, 2.5), mdl_value = c(2.0, 2.0, 2.0),
                       mdl_action = "zero"),
               c(0, 0, 2.5))
  expect_equal(set_non_detects(c(NA, 2.0, 2.5), mdl_flag = c("<", "<", NA), mdl_action = "zero"),
               c(NA, 0, 2.5))
  expect_equal(set_non_detects(c(NA, 2.0, 2.5), mdl_value = c(2.0, 2.0, 2.0),
                       mdl_action = "zero"),
               c(NA, 0, 2.5))
})

test_that("set_non_detects works with half mdl", {
  expect_equal(set_non_detects(c(1.5, 2.0, 2.5), mdl_flag = c("<", NA, NA), mdl_action = "half"),
               c(0.75, 2.0, 2.5))
  expect_equal(set_non_detects(c(1.5, 2.0, 2.5), mdl_value = c(1.9, 1.9, 1.9),
                       mdl_action = "half"),
               c(0.95, 2.0, 2.5))
  expect_equal(set_non_detects(c(1.5, 2.0, 2.5), mdl_value = c(2.0, 2.0, 2.0),
                       mdl_action = "half"),
               c(1.0, 1.0, 2.5))
  expect_equal(set_non_detects(c(NA, 2.0, 2.5), mdl_flag = c("<", "<", NA), mdl_action = "half"),
               c(NA, 1.0, 2.5))
  expect_equal(set_non_detects(c(NA, 2.0, 2.5), mdl_value = c(2.0, 2.0, 2.0),
                       mdl_action = "half"),
               c(NA, 1.0, 2.5))
})

test_that("set_non_detects works with mdl", {
  expect_equal(set_non_detects(c(1.5, 2.0, 2.5), mdl_flag = c("<", NA, NA), mdl_action = "mdl"),
               c(1.5, 2.0, 2.5))
  expect_equal(set_non_detects(c(1.5, 2.0, 2.5), mdl_value = c(1.9, 1.9, 1.9),
                       mdl_action = "mdl"),
               c(1.9, 2.0, 2.5))
  expect_equal(set_non_detects(c(1.5, 2.0, 2.5), mdl_value = c(2.0, 2.0, 2.0),
                       mdl_action = "mdl"),
               c(2.0, 2.0, 2.5))
  expect_equal(set_non_detects(c(NA, 2.0, 2.5), mdl_flag = c("<", "<", NA), mdl_action = "mdl"),
               c(NA, 2.0, 2.5))
  expect_equal(set_non_detects(c(NA, 2.0, 2.5), mdl_value = c(2.0, 2.0, 2.0),
                       mdl_action = "mdl"),
               c(NA, 2.0, 2.5))
})

test_that("tidy_ems_data works", {
  tidied_ems <- tidy_ems_data(test_ems)
  expect_equal(names(tidied_ems),
               c("EMS_ID", "Station", "DateTime", "Variable", "Code", "Value",
                 "Units", "DetectionLimit", "ResultLetter"))
})

test_that("tidy_ec_data works", {
  tidied_ec <- tidy_ec_data(test_ec)
  expect_equal(names(tidied_ec),
               c("Station", "DateTime", "Variable", "Code", "Value",
                 "DetectionLimit", "Units"))
})
