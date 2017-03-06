#' Tidy EMS Data
#'
#' Tidies water quality data downloaded from EMS database using the bcgov/rems package.
#' It retains and renames required columns and sets the timezone to PST.
#'
#' It sets values that are flagged as being less than the detection limit to zero.
#' It does not alter values that are flagged as being greater than the detection limit -
#' that is left up to the user.
#'
#' @param x The rems data to tidy.
#' @param cols additional columns from the EMS data to retain.
#' @return A tibble of the tidied rems data.
#' @export
tidy_ems_data <- function(x, cols = character(0)) {
  check_cols(x, c("EMS_ID", "MONITORING_LOCATION", "COLLECTION_START", "PARAMETER_CODE",
                  "RESULT", "UNIT", "METHOD_DETECTION_LIMIT", "PARAMETER", "RESULT_LETTER", cols))

  x %<>% dplyr::mutate_(DateTime = ~lubridate::force_tz(COLLECTION_START, "Etc/GMT+8"))

  x %<>% dplyr::select_(~EMS_ID,
                        Station = ~MONITORING_LOCATION,
                        ~DateTime,
                        Variable = ~PARAMETER,
                        Code = ~PARAMETER_CODE,
                        Value = ~RESULT,
                        Units = ~UNIT,
                        DetectionLimit = ~METHOD_DETECTION_LIMIT,
                        ResultLetter = ~RESULT_LETTER)

  x$Value[!is.na(x$Value) & !is.na(x$ResultLetter) & x$ResultLetter == "<"] <- 0

  x
}

#' Tidy Environment Canada Data
#'
#' Tidies water quality data downloaded from Environment Canada website.
#' It retains and renames required columns and sets the timezone to PST.
#'
#' @param x The rems data to tidy.
#' @return A tibble of the tidied rems data.
#' @export
tidy_ec_data <- function(x) {
  check_cols(x, c("SITE_NO", "DATE_TIME_HEURE", "VALUE_VALEUR", "SDL_LDE", "VMV_CODE", "VARIABLE"))

  unit <- dplyr::select_(x, ~starts_with("UNIT_UNIT"))

  if (ncol(unit) != 1) error("x must include a column starting with UNIT_UNIT")

  x %<>% dplyr::select_(Station = ~SITE_NO,
                        DateTime = ~DATE_TIME_HEURE,
                        Variable = ~VARIABLE,
                        Code = ~VMV_CODE,
                        Value = ~VALUE_VALEUR,
                        DetectionLimit = ~SDL_LDE)

  x$Units <- unit[[1]]

  x %<>% dplyr::select_(~dplyr::everything(), ~DetectionLimit)

  x$Value[!is.na(x$Value) & !is.na(x$DetectionLimit) & x$DetectionLimit > 0 & x$Value <= x$DetectionLimit] <- 0

  x %<>% dplyr::mutate_(DateTime = ~lubridate::dmy_hm(DateTime, tz = "Etc/GMT+8"))

  x
}
