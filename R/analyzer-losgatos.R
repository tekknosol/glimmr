
#' LosGatos setup
#'
#' Explicitly create LosGatos setup. Usually you will not call this
#' function, but will instead use the wrapper \code{\link{process_losgatos}}.
#'
#' @inheritParams analyzer
#'
#' @export
#'
#'
analyzer_LosGatos <- function(
  manual_temperature = NULL,
  offset = "offset",
  duration_count = FALSE,
  plot = "plot",
  date = "date",
  start = "start",
  end = "end",
  trimmer = trim_time,
  V = 0.01461,
  A = 0.098
){
  me <- analyzer(
    time_stamp = "Time",
    conc_columns = c(CH4 = "[CH4]_ppm", CO2 = "[CO2]_ppm"),
    preassure = "GasP_torr",
    preassure_factor = 1.33322,
    temperature = "AmbT_C",
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
  # me <- do.call(lyzr,c(settings))
  ## Add the name for the class
  class(me) <- append(class(me),"LosGatos")
  me
}
