#' GASMET setup
#'
#' Explicitly create GASMET setup. Usually you will not call this
#'   function, but will instead use the wrapper \code{\link{process_gasmet}}
#'
#' @inheritParams analyzer
#'
#' @export
#'
#'
analyzer_GASMET <- function(
  manual_temperature = "temp",
  offset = "offset",
  duration_count = TRUE,
  plot = "plot",
  date = "date",
  start = "start",
  end = 10,
  trimmer = NULL,
  V = 0.01461,
  A = 0.098
){
  me <- analyzer(
    time_stamp = "datetime",
    conc_columns = c(CO2 = "CO2", CH4 = "CH4", N2O = "N2O"),
    pressure = "Luftdruck",
    temperature = NA,
    manual_temperature = manual_temperature,
    offset = offset,
    duration_count = duration_count,
    plot = plot,
    date = date,
    start = start,
    end = end,
    trimmer = trimmer,
    V = V,
    A = A
  )

  class(me) <- append(class(me),"GASMET")
  me
}
