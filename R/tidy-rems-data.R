#' Tidy Rems Data
#'
#' Tidies data from rems by retaining and renaming required columns and setting the
#' timezone to PST.
#'
#' @param x The rems data to tidy.
#' @return A tibble of the tidied rems data.
#' @export
tidy_rems_data <- function(x) {
  check_cols(x, c("EMS_ID", "COLLECTION_START", "PARAMETER_CODE",
                  "RESULT", "UNIT", "METHOD_DETECTION_LIMIT", "PARAMETER",
                  "LATITUDE", "LONGITUDE"))

  x %<>% dplyr::mutate_(DateTime = ~lubridate::force_tz(COLLECTION_START, "Etc/GMT+8"))

  x %<>% dplyr::select_(~EMS_ID, ~DateTime, Code = ~PARAMETER_CODE,
                          Value = ~RESULT, Units = ~UNIT,
                          DetectionLimit = ~METHOD_DETECTION_LIMIT, Variable = ~PARAMETER,
                          Latitude = ~LATITUDE, Longitude = ~LONGITUDE)

  x$Value[!is.na(x$Value) & !is.na(x$DetectionLimit) & x$DetectionLimit > 0 & x$Value <= x$DetectionLimit] <- 0

  x
}
