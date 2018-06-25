

preprocess_chamber <- function(conc, meta, device, V, A){
  chamber_diagnostic(conc, meta, device)
  hmr_data <- dplyr::tibble(
    spot = NA, day = NA, rep = NA, start = NA, V = NA, A = NA, Time = NA
  )

  for (col in device$conc_columns){
    gas <- names(device$conc_columns)[device$conc_columns == col]
    colname <- paste(gas, "mmol", sep="")
    hmr_data <- hmr_data %>%
      tibble::add_column(!!colname := NA, !!gas := NA)
  }

  repcount <- data.frame(spot = unique(meta$spot), count = 0)
  for (i in 1:length(meta$spot)) {
    suppressWarnings(
      start <- lubridate::ymd_hms(paste(meta[i, ] %>% dplyr::pull(device$day), meta[i, ]%>% dplyr::pull(device$start)))
    )
      end <- parse_end(conc, device, start, meta[i, ])

    int <- lubridate::interval(start, end)
    if(!is.na(device$time_proc)){
      func <- match.fun(device$time_proc)
      int <- func(int)
    }

    repcount[repcount$spot == meta[i, ] %>% dplyr::pull(device$spot), ]$count <- repcount[repcount$spot == meta[i, ] %>% dplyr::pull(device$spot), ]$count + 1
    a <- conc %>%
      dplyr::filter(!!rlang::sym(device$time_stamp) >= lubridate::int_start(int) & !!rlang::sym(device$time_stamp) <= lubridate::int_end(int))
    if (length(rownames(a)) == 0){
      warning(call. = FALSE, "meta entry ", i, " skipped.",
              " No matching data.")
      next
    }

    if (is.na(device$manual_temperature)){
      temp_value <- parse_var(a, meta[i, ], device$temperature)
    } else{
      temp_value <- parse_var(a, meta[i, ], device$manual_temperature)
    }
    preassure_value <- parse_var(a, meta[i, ], device$preassure) * device$preassure_factor

    hmr_data_tmp <- data.frame(
      spot = meta[i, ] %>% dplyr::pull(device$spot), day =
      as.character(meta[i, ] %>% dplyr::pull(device$day)), rep =
      repcount[repcount$spot == meta[i, ] %>% dplyr::pull(device$spot), ]$count,
      start = as.character(lubridate::int_start(int)), V = V, A = A, Time = as.numeric(a %>% dplyr::pull(device$time_stamp) - a[1,] %>% dplyr::pull(device$time_stamp)) / 60 / 60
    )

    # calculate concentration in mmol/m³
    for (col in device$conc_columns){
      # colname <- paste("conc_", col, sep="")
      gas <- names(device$conc_columns)[device$conc_columns == col]
      colname <- paste(gas, "mmol", sep="")
      hmr_data_tmp <- hmr_data_tmp %>%
        tibble::add_column(!!colname := ppm2conc(a %>% dplyr::pull(!!col), temp_value, preassure_value), !!gas := gas)
    }
    hmr_data <- rbind(hmr_data, hmr_data_tmp)
  }
  hmr_data <- hmr_data[-1, ]
}

#' Calculate gasfluxes from dynamic chamber measurement
#'
#' `process_losgatos()`and `process_gasmet()` are special cases of the general
#' `process_chamber()`.
#'
#' @param data Data frame. Recorded data from device.
#' @param meta Data frame. Metadata containing required informations.
#' @param time_stamp Character vector. Value will be used as the column in conc
#'   containing the timestamp.
#' @param conc_columns A named character vector. Values will be used to extract
#'   ppm data from conc. E.g. c(CO2 = "CO2").
#' @param preassure Character.
#' @param preassure_factor Numeric value to convert preassure to mbar.
#' @param temperature Character.
#' @param manual_temperature Character.
#' @param spot Character.
#' @param day Character.
#' @param start Character.
#' @param duration_count Is `end` given as time information or number of
#'   datapoints? If FALSE (the default), `end` will be interpreted as Time or duration in
#'   minutes. If TRUE, `end` will be interpreted as number of datapoints per
#'   chamber application.
#' @param end Character.
#' @param time_proc NA or name of function. Function is used to modify interval
#'   between `start` and `end`.
#' @param V Numeric. Volume of used chamber.
#' @param A Numeric. Area of used chamber.
#' @param pre If FALSE (the default), flux processing will be executed. If TRUE flux
#'   processing will be skipped and preprocessed data frame will be returned.
#'
#' @return A data frame.
#' @export
#'
process_chamber <- function(data, meta, time_stamp, conc_columns, preassure,
                            preassure_factor = 1, temperature = "temp",
                            manual_temperature = NA, duration_count = FALSE,
                            spot = "spot", day = "day", start = "start",
                            end = "end", V, A, time_proc = NA, pre = FALSE){
  device <- device_generic(time_stamp = time_stamp, conc_columns = conc_columns,
    preassure = preassure, preassure_factor = preassure_factor, temperature =
    temperature, manual_temperature = manual_temperature, duration_count =
    duration_count, spot = spot, day = day, start = start, end = end,
    time_proc = time_proc
  )
  hmr_data <- preprocess_chamber(data, meta, device, V, A)

  if (pre == TRUE) {
    return(hmr_data)
  }

  flux <- process_flux(hmr_data, meta, device)
  return(flux)
}

#' @rdname process_chamber
#' @export
process_losgatos <- function(data, meta, manual_temperature = NA, spot = "spot", day = "day",
                             start = "start", end = "end", V = 0.01461,
                             A = 0.098, pre = FALSE){
  device <- device_losgatos(manual_temperature = manual_temperature, spot = spot, day = day,
    start = start, end = end)
  hmr_data <- preprocess_chamber(data, meta, device, V, A)

  if (pre == TRUE) {
    return(hmr_data)
  }

  flux <- process_flux(hmr_data, meta, device)
  return(flux)
}

#' @rdname process_chamber
#' @export
process_gasmet <- function(data, meta, manual_temperature = "temp", spot = "spot", day = "day",
                           start = "start", end = "wndw", V = 0.01461,
                           A = 0.098, pre = FALSE){
  device <- device_gasmet(manual_temperature = manual_temperature, spot = spot, day = day,
    start = start, end = end)
  hmr_data <- preprocess_chamber(data, meta, device, V, A)

  if (pre == TRUE) {
    return(hmr_data)
  }

  flux <- process_flux(hmr_data, meta, device)
  return(flux)
}
