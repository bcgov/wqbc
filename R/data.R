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

#' Water Quality Guidelines for British Columbia and Canada
#'
#' Both the Canadian federal government and the
#' province of British Columbia set guidelines for a range of
#' water quality parameters. The data were taken from a
#' range of federal and provincial websites. They represent
#' long-term guidelines for samples taken from the water column.
#' Guidelines which depend on conditions from other periods
#' or locations are not included. For example some of the
#' guidelines for turbidity are defined with respect to background
#' levels and/or an upstream site.
#' Where some interpretation of the information provided was required
#' this is noted in the comments. Missing values (NA) are permitted in
#' Form, Condition and Comments only.
#'
#' @format A data frame with 13 variables:
#' \describe{
#'   \item{Parameter}{name of water quality parameter}
#'   \item{Form}{form of parameter (permitted values: Dissolved, Total, E.coli, Enterococci)}
#'   \item{Jurisdiction}{regulatory jurisdiction (permitted values:
#'   British Columbia, Canada)}
#'   \item{Use}{intended use (permitted values: Drinking, Freshwater Life,
#'    Marine Life, Wildlife, Livestock, Irrigation, Recreation)}
#'   \item{Samples}{minimum number of samples required}
#'   \item{Days}{period within which number of samples must be collected}
#'   \item{Average}{R expression to calculate "average" value for multiple samples}
#'   \item{Condition}{R logical expression to test required condition}
#'   \item{Guideline}{R logical expression to test whether guideline fulfilled}
#'   \item{Unit}{units for guideline (permitted values: mg/L, ug/L, /100mL, m, C, NTU)}
#'   \item{Comments}{comments regarding the interpretation of the online documentation}
#'   \item{Date}{YYYY-MM-DD online documentation last checked}
#'   \item{URL}{online documentation from which guideline extracted}
#' }
"guidelines"
