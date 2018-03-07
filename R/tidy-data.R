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
#' * "PARAMETER_CODE (Renamed to "Code")
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
  check_cols(x, c("EMS_ID", "MONITORING_LOCATION", "COLLECTION_START", "PARAMETER_CODE",
                  "RESULT", "UNIT", "METHOD_DETECTION_LIMIT", "PARAMETER", "RESULT_LETTER",
                  "SAMPLE_STATE", "SAMPLE_CLASS", "SAMPLE_DESCRIPTOR", cols))

  x %<>% dplyr::mutate_(DateTime = ~lubridate::force_tz(COLLECTION_START, "Etc/GMT+8"))

  x %<>% dplyr::select(.data$EMS_ID,
                        Station = .data$MONITORING_LOCATION,
                        .data$DateTime,
                        Variable = .data$PARAMETER,
                        Code = .data$PARAMETER_CODE,
                        Value = .data$RESULT,
                        Units = .data$UNIT,
                        DetectionLimit = .data$METHOD_DETECTION_LIMIT,
                        ResultLetter = .data$RESULT_LETTER,
                       .data$SAMPLE_STATE,
                       .data$SAMPLE_CLASS,
                       .data$SAMPLE_DESCRIPTOR,
                       !!cols)

  x$Value <- set_non_detects(value = x$Value,
                            mdl_flag = x$ResultLetter,
                            mdl_action = mdl_action)

  x
}

#' Tidy Environment Canada Data
#'
#' Tidies water quality data downloaded from Environment Canada website.
#' It retains and renames required columns and sets the timezone to PST.
#'
#' @param x The rems data to tidy.
#' @param mdl_action What to do with results that are below the detection limit.
#' Can be set to \code{zero} (the default), set at the detection limit (\code{mdl}),
#' or set to half the detection limit (\code{half}).
#' @return A tibble of the tidied rems data.
#' @export
tidy_ec_data <- function(x, mdl_action = "zero") {
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

  if (inherits(x$DateTime, "POSIXt")) {
    x$DateTime <- lubridate::force_tz(x$DateTime, "Etc/GMT+8")
  } else {
    x %<>% dplyr::mutate_(DateTime = ~lubridate::dmy_hm(DateTime, tz = "Etc/GMT+8"))
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
#' @param mdl_flag a character vector the same length as \code{value} that has a "flag" (assumed to be \code{"<"}) for values that are below the MDL
#' @param mdl_value a numeric vector the same length as \code{value} that contains the MDL values.
#' @param mdl_action What to do with values below the detection limit. Options are
#' \code{"zero"} (set the value to \code{0}), \code{"half"} (set the value to half the MDL), or \code{"mdl"} (set the value to equal to the MDL).
#'
#' @details You must only supply either \code{mdl_flag} or \code{mdl_value}.
#' When \code{mdl_flag} is supplied, it is assumed that the original \code{value} has
#' been set to the MDL.
#'
#' @return a numeric vector the same length as value with non-detects adjusted accordingly
#' @export
set_non_detects <- function(value, mdl_flag = NULL, mdl_value = NULL, mdl_action = c("zero", "half", "mdl")) {

  mdl_action <- match.arg(mdl_action)

  if (!is.null(mdl_flag)) {

    if (!is.null(mdl_value)) stop ("You must supply only one of mdl_flag or mdl_value")
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
