
device_generic <- function(time_stamp, conc_columns, preassure, temperature, duration_count = FALSE, count = NA){
  structure(
    list(
      time_stamp = time_stamp,
      conc_columns = conc_columns,
      preassure = preassure,
      temperature = temperature,
      duration_count = duration_count
    ),
    class = "fluxdevice"
  )
}


device_losgatos <- function(temperature = "AmbT_C"){
  device_generic(
    time_stamp = "Time",
    conc_columns = c(CH4 = "[CH4]_ppm", CO2 = "[CO2]_ppm"),
    preassure = "GasP_torr",
    temperature = temperature
  )
}

device_gasmet <- function(temperature = "temp", duration_count = TRUE, count = 10){
  device_generic(
    time_stamp = c("datetime"),
    conc_columns = c(CH4 = "CH4", CO2 = "CO2"),
    preassure = "Luftdruck",
    temperature = temperature,
    duration_count = duration_count,
    count = count
  )
}
