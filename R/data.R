#' GASMET Flux measurement with three entries.
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

#' LosGatos Flux measurement with three entries.
#'
#' A dataset containing one flux measurements.
#'
#' @format A data frame with 1684 rows and 5 variables:
#' \describe{
#'   \item{Time}{Datetime of measurement}
#'   \item{[CH4]_ppm}{}
#'   \item{[CO2]_ppm}{}
#'   \item{GasP_torr}{}
#'   \item{AmbT_C}{}
#' }
"losgatos"

#' Metafile for LosGatos measurement.
#'
#' A dataset containing the meta information for one flux measurements.
#'
#' @format A data frame with 1 rows and 19 variables:
#' \describe{
#'   \item{spot}{ID of the measurement spot}
#'   \item{day}{Day of the measurement}
#'   \item{start}{}
#'   \item{end}{}
#'   \item{t_start}{}
#'   \item{t_end}{}
#' }
"meta_losgatos"

#' Metafile for GASMET measurement.
#'
#' A dataset containing the meta information for the three flux measurements.
#'
#' @format A data frame with 3 rows and 19 variables:
#' \describe{
#'   \item{spot}{ID of the measurement spot}
#'   \item{day}{Day of the measurement}
#'   \item{start}{}
#'   \item{temp}{}
#'   \item{wndw}{}
#'   \item{offset}{}
#' }
"meta_gasmet"
