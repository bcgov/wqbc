#' Fraser River Basin Long-term Water Quality Monitoring 1979-Present
#'
#' Surface freshwater quality monitoring in the Fraser River Basin 
#' is carried out under the Canada-British Columbia Water Quality 
#' Monitoring Agreement. Monitoring is conducted to assess water 
#' quality status and long-term trends, detect emerging issues, 
#' establish water quality guidelines and track the effectiveness 
#' of remedial measures and regulatory decisions.
#'
#' @format A data frame with 249079 rows and 12 variables:
#' \describe{
#'   \item{station_no}{unique water quality station number}
#'   \item{sample_datetime}{ISO 8601 time of captured water quality sample}
#'   \item{value}{value of observed measurement}
#'   \item{method_detect_limit}{lower measurable limit of method}
#'   \item{unit_code}{unit of variable measured}
#'   \item{vmv_code}{unique variable and method code}
#'   \item{variable_name}{observed phenonomenon variable name}
#'   \item{flag}{measurement quality assurance flag by laboratory}
#'   \item{station_name}{full station name}
#'   \item{latitude}{latitude in decimal degrees}
#'   \item{longitude}{longitude in decimal degrees}
#'   \item{status}{post-analysis quality assurance flag}
#' }
#' @source \url{http://open.canada.ca/data/en/dataset/9ec91c92-22f8-4520-8b2c-0f1cce663e18}
"waterq"