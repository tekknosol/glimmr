
device_generic <- function(name = "generic", time_stamp, conc_columns, preassure,
                           preassure_factor = 1, temperature,
                           manual_temperature = NA, duration_count = FALSE,
                           spot = "spot", day = "day", start = "start",
                           end = "end", time_proc = NA){
  structure(
    list(
      name = name,
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
      time_proc = time_proc
    ),
    class = "fluxdevice"
  )
}


device_losgatos <- function(manual_temperature = NA, spot = "spot", day = "day",
                            start = "start", end = "end"){
  device_generic(
    name = "losgatos",
    time_stamp = "Time",
    conc_columns = c(CH4 = "[CH4]_ppm", CO2 = "[CO2]_ppm"),
    preassure = "GasP_torr",
    preassure_factor = 1.33322,
    temperature = "AmbT_C",
    manual_temperature = manual_temperature,
    spot = spot,
    day = day,
    start = start,
    end = end,
    time_proc = trim_time
  )
}

device_gasmet <- function(manual_temperature = "temp",
                          spot = "spot", day = "day", start = "start",
                          end = "wndw"){
  device_generic(
    name = "gasmet",
    time_stamp = c("datetime"),
    conc_columns = c(CH4 = "CH4", CO2 = "CO2"),
    preassure = "Luftdruck",
    temperature = NA,
    manual_temperature = manual_temperature,
    duration_count = TRUE,
    spot = spot,
    day = day,
    start = start,
    end = end,
    time_proc = NA
  )
}
