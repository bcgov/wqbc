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
#' @param cols additional columns from the EMS data to retain specified as a
#' character vector of column names that exist in the data.
#' The dafault columns retained are:
#' * "EMS_ID"
#' * "MONITORING_LOCATION" (Renamed to "Station")
#' * "COLLECTION_START" (Renamed to "DateTime")
#' * "PARAMETER" (Renamed to "Variable")
#' * "PARAMETER_CODE" (Renamed to "Code")
#' * "RESULT" (Renamed to "Value")
#' * "UNIT" (Renamed to "Units")
#' * "METHOD_DETECTION_LIMIT" (Renamed to "DetectionLimit")
#' * "RESULT_LETTER" (Renamed to "ResultLetter")
#' * "SAMPLE_STATE"
#' * "SAMPLE_CLASS"
#' * "SAMPLE_DESCRIPTOR"
#' @param mdl_action What to do with results that are below the detection limit.
#' Can be set to \code{zero} (the default), set at the detection limit (\code{mdl}),
#' or set to half the detection limit (\code{half}).
#'
#' @return A tibble of the tidied rems data.
#' @export
tidy_ems_data <- function(x, cols = character(0), mdl_action = "zero") {
  cols <- c("EMS_ID",
            "Station" = "MONITORING_LOCATION",
            "DateTime" = "COLLECTION_START",
            "Variable" = "PARAMETER",
            "Code" = "PARAMETER_CODE",
            "Value" = "RESULT",
            "Units" = "UNIT",
            "DetectionLimit" = "METHOD_DETECTION_LIMIT",
            "ResultLetter" = "RESULT_LETTER",
            "SAMPLE_STATE",
            "SAMPLE_CLASS",
            "SAMPLE_DESCRIPTOR",
            "LOCATION_TYPE",
            cols)

  check_cols(x, unname(cols))
  cols <- rlang::syms(cols)
  x <- dplyr::select(x, !!!cols)

  x$DateTime <- lubridate::force_tz(x$DateTime, tzone = "Etc/GMT+8")

  x$Value <- set_non_detects(value = x$Value,
                             mdl_flag = x$ResultLetter,
                             mdl_action = mdl_action)

  x
}

#' Tidy Environment Canada Data
#'
#' Tidies water quality data downloaded from Environment Canada website. It
#' is recommended to obtain the data via [canwqdata::dl_sites()] or
#' [canwqdata::dl_basin()]
#' It retains and renames required columns and sets the timezone to PST.
#'
#' @param x The data to tidy.
#' @param cols additional columns from the EMS data to retain specified as a
#' character vector of column names that exist in the data.
#' The dafault columns retained are:
#' * "SITE_NO"
#' * "DATE_TIME_HEURE" (Renamed to "DateTime")
#' * "VARIABLE" (Renamed to "Variable")
#' * "VMV_CODE" (Renamed to "Code")
#' * "VALUE_VALEUR" (Renamed to "Value")
#' * "UNIT_UNITE" (Renamed to "Units")
#' * "DSL_LDE" (Renamed to "DetectionLimit")
#' * "FLAG_MARQUEUR" (Renamed to "ResultLetter")
#' @param mdl_action What to do with results that are below the detection limit.
#' Can be set to \code{zero} (the default), set at the detection limit (\code{mdl}),
#' or set to half the detection limit (\code{half}).
#' @return A tibble of the tidied rems data.
#' @export
tidy_ec_data <- function(x, cols = character(0), mdl_action = "zero") {
  cols <-  c("SITE_NO",
             "DateTime" = "DATE_TIME_HEURE",
             "Variable" = "VARIABLE",
             "Code" = "VMV_CODE",
             "Value" = "VALUE_VALEUR",
             "Units" = "UNIT_UNITE",
             "DetectionLimit" = "SDL_LDE",
             "ResultLetter" = "FLAG_MARQUEUR",
             cols)

  check_cols(x, unname(cols))
  cols <- rlang::syms(cols)
  x <- dplyr::select(x, !!!cols)

  if (inherits(x$DateTime, "POSIXt")) {
    x$DateTime <- lubridate::force_tz(x$DateTime, tzone = "Etc/GMT+8")
  } else {
    x$DateTime <- lubridate::dmy_hm(x$DateTime, tz = "Etc/GMT+8")
  }

  x$Value <- set_non_detects(value = x$Value,
                             mdl_value = x$DetectionLimit,
                             mdl_action = mdl_action)

  x
}

#' Set value for 'non-detects'
#'
#' Set a value where the actual value of a measurement
#' is less than the method detection limit (MDL)
#'
#' @param value a numeric vector of measured values
#' @param mdl_flag a character vector the same length as `value` that has a
#' "flag" (assumed to be `"<"`) for values that are below the MDL
#' @param mdl_value a numeric vector the same length as `value` that contains
#' the MDL values.
#' @param mdl_action What to do with values below the detection limit. Options
#' are `"zero"` (set the value to `0`; the default), #' `"half"` (set the value
#' to half the MDL), `"mdl"` (set the value to equal to the MDL), or `"na"` (set
#' the value to `NA`).
#'
#' @details You must only supply either `mdl_flag` or `mdl_value`.
#' When `mdl_flag` is supplied, it is assumed that the original `value` has
#' been set to the MDL.
#'
#' @return a numeric vector the same length as value with non-detects adjusted accordingly
#' @export
set_non_detects <- function(value, mdl_flag = NULL, mdl_value = NULL,
                            mdl_action = c("zero", "mdl", "half", "na")) {

  mdl_action <- match.arg(mdl_action)

  if (!is.null(mdl_flag)) {

    if (!is.null(mdl_value)) stop("You must supply only one of mdl_flag or mdl_value")
    if (length(value) != length(mdl_flag)) {
      stop("value and mdl_flag must be the same length")
    }

    replacements <- !is.na(value) & !is.na(mdl_flag) & mdl_flag == "<"

  } else if (!is.null(mdl_value)) {

    if (length(value) != length(mdl_value)) {
      stop("value and mdl_value must be the same length")
    }

    replacements <- !is.na(value) & !is.na(mdl_value) & mdl_value > 0 & value <= mdl_value

  } else {
    stop("You must supply either a mdl_flag vector, or a mdl_value vector")
  }

  if (mdl_action == "zero") {
    value[replacements] <- 0
  } else if (mdl_action == "na") {
    value[replacements] <- NA
  } else {

    multiplier <- ifelse(mdl_action == "half", 0.5, 1)

    if (!is.null(mdl_flag)) {
      value[replacements] <- value[replacements] * multiplier
    } else {
      value[replacements] <- mdl_value[replacements] * multiplier
    }

  }

  value

}
