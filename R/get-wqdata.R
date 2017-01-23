#' Get WQ Data
#'
#' A wrapper on rems functions to get EMS data from
#'
#' @inheritParams rems::download_historic_data
#' @inheritParams rems::read_historic_data
#' @seealso \code{\link[rems]{download_historic_data}} and \code{\link[rems]{read_historic_data}}
#'
#' @return A data.frame of water quality data suitable for use by wqbc functions.
#' @export
get_wqdata <- function(emsid = NULL, param_code = NULL, from_date = NULL, to_date = NULL, force = FALSE, ask = TRUE) {
  check_flag(force)
  check_flag(ask)

  cols <- c("EMS_ID", "COLLECTION_START", "LATITUDE", "LONGITUDE",
            "PARAMETER_CODE", "PARAMETER", "RESULT", "UNIT", "METHOD_DETECTION_LIMIT")

  rems::download_historic_data(force = force, ask = ask)

  ems_historic <- rems::read_historic_data(emsid = emsid, param_code = param_code,
                                     from_date = from_date, to_date = to_date, cols = cols)

  ems <- rems::get_ems_data(cols = cols, force = force, ask = ask)

  ems %<>% rems::filter_ems_data(emsid = emsid, param_code = param_code,
                           from_date = from_date, to_date = to_date)

  ems <- dplyr::bind_rows(ems_historic, .)

  ems %<>% dplyr::select_(~EMS_ID, Date = ~COLLECTION_START, Code = ~PARAMETER_CODE,
                  Value = ~RESULT, Units = ~UNIT,
                  DetectionLimit = ~METHOD_DETECTION_LIMIT, Variable = ~PARAMETER,
                  Lat = ~LATITUDE, Long = ~LONGITUDE)

  # convert ems$Date from POSIX to Date
  ems$Date %<>% lubridate::date()

  ems
}

