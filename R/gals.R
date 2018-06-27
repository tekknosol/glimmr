#' Gas analyzer setup
#'
#' @param name Name of device. Defaults to "custom"
#' @param fixed_params Named vector.
#' @param time_stamp Character vector. Value will be used as the column in conc
#'   containing the timestamp.
#' @param conc_columns A named character vector. Values will be used to extract
#'   ppm data from conc. E.g. c(CO2 = "CO2").
#' @param preassure Character
#' @param preassure_factor Numeric value to convert preassure to mbar.
#' @param temperature Character
#' @param manual_temperature Character
#' @param duration_count Is `end` given as time information or number of
#'   datapoints? If FALSE (the default), `end` will be interpreted as Time or
#'   duration in minutes. If TRUE, `end` will be interpreted as number of
#'   datapoints per chamber application.
#' @param spot Character.
#' @param day Character.
#' @param start Character.
#' @param end Character.
#' @param trimmer NULL or function. Function is used to modify interval between
#'   `start` and `end`.
#' @param V Numeric. Volume of used chamber.
#' @param A Numeric. Area of used chamber.
#'
#' @return A gals object.
#' @export
gals <- function(name = "custom", fixed_params = c(), time_stamp = NULL,
                 conc_columns = NULL, preassure = NULL, preassure_factor = 1,
                 temperature = NULL, manual_temperature = NULL, duration_count = FALSE,
                 spot = "spot", day = "day", start = "start", end = "end", trimmer = NULL, V = 0.01461, A = 0.098){
  structure(
    list(
      name = name,
      fixed_params = fixed_params,
      time_stamp = time_stamp,
      conc_columns = conc_columns,
      preassure = preassure,
      preassure_factor = preassure_factor,
      temperature = temperature,
      manual_temperature = manual_temperature,
      duration_count = duration_count,
      spot = spot,
      day = day,
      start = start,
      end = end,
      trimmer = as.character(substitute(trimmer)),
      V = V,
      A = A
    ),
    class = "gals"
  )
}


#' LosGatos setup
#'
#' Explicitly create LosGatos setup. Usually you will not call these
#' function, but will instead use the wrapper [process_losgatos()]
#'
#' @export
#'
#'
#' @eval rd_gals("losgatos")
gals_losgatos <- function(){
  gals(
    fixed_params = c("name", "time_stamp", "conc_columns", "preassure", "preassure_factor", "temperature"),
    name = "losgatos",
    time_stamp = "Time",
    conc_columns = c(CH4 = "[CH4]_ppm", CO2 = "[CO2]_ppm"),
    preassure = "GasP_torr",
    preassure_factor = 1.33322,
    temperature = "AmbT_C",
    trimmer = trim_time()
  )
}

#' GASMET setup
#'
#' Explicitly create LosGatos setup. Usually you will not call these
#'   function, but will instead use the wrapper [process_gasmet()]
#'
#' @export
#'
#'
#' @eval rd_gals("gasmet")
gals_gasmet <- function(){
  gals(
    fixed_params = c("name", "time_stamp", "conc_columns", "preassure", "temperature", "preassure_factor"),
    name = "gasmet",
    time_stamp = "datetime",
    conc_columns = c(CH4 = "CH4", CO2 = "CO2", N2O = "N2O"),
    preassure = "Luftdruck",
    temperature = NA,
    manual_temperature = "temp",
    duration_count = TRUE,
    end = "wndw"
  )
}

update.gals <- function(gal, gal2){
  locked <- names(gal)[which(names(gal) %in% gal$fixed_params)]
  pos <- which(!names(gal2) %in% gal$fixed_params)
  gal2 <- gal2[pos]
  gal <- utils::modifyList(gal, gal2)
  return(gal)
}

is.gals <- function(x) inherits(x, "gals")
