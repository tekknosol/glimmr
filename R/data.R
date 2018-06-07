#' GASMET Flux measurement with three entrys.
#'
#' A dataset containing three flux measurements.
#'
#' @format A data frame with 25 rows and 7 variables:
#' \describe{
#'   \item{Datum}{Date of measurement}
#'   \item{Zeit}{Time of the datapoint}
#'   \item{CO2}{}
#'   \item{CH4}{}
#'   \item{N2O}{}
#'   \item{Luftdruck}{}
#'   \item{datetime}{}
#' }
"gasmet"


#' Metafile for GASMET measurement.
#'
#' A dataset containing the meta information for the three flux measurements.
#'
#' @format A data frame with 3 rows and 19 variables:
#' \describe{
#'   \item{spot}{ID of the measurement spot}
#'   \item{day}{Day of the measurement}
#'   \item{begin}{}
#'   \item{temp}{}
#'   \item{wndw}{}
#'   \item{offset}{}
#' }
"meta_gasmet"
