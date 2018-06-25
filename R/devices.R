
preprocess_gasmet <- function(gasmet, meta, V = 0.01461,
                              A = 0.098, wndw = 10, offset = 0) {
  terminate <- F
  if (T %in% is.na(meta$temp)) {
    terminate <- T
  }
  if (terminate) {
    stop("missing temperature values in meta file. ",
      "Flux calculation stoped.")
  }

  if (T %in% is.na(meta$wndw) | is.null(meta$wndw)) {
    meta$wndw <- wndw
    warning("No window provided in meta file. Set to 10.")
  }

  if (T %in% is.na(meta$offset) | is.null(meta$offset)) {
    meta$offset <- offset
    warning("No offset provided in meta file. Set to 0.")
  }

  hmr_data <- dplyr::tibble(
    spot = NA, day = NA, rep = NA, V = NA, A = NA,
    Time = NA, CO2mmol = NA, CH4mmol = NA,
    N2Ommol = NA, co2 = NA, ch4 = NA, n2o = NA
  )
  repcount <- data.frame(spot = unique(meta$spot), count = 0)
  for (i in 1:length(meta$spot)) {
    if (!is.na(meta[i, ]$begin)) {
      repcount[repcount$spot == meta[i, ]$spot, ]$count <- repcount[repcount$spot == meta[i, ]$spot, ]$count + 1
      begin <- which(gasmet$datetime == meta[i, ]$begin)
      if (length(begin) == 0) {
        warning(call. = FALSE, "meta entry ", i, " skipped.",
          " Begin does not match data.")
        next
      }
      a <- gasmet[(begin + meta[i, ]$offset):
      (begin + meta[i, ]$wndw - 1), ]
      pmbar <- mean(a$Luftdruck)
      # calculate concentration in mmol/m³
      conc     <- ppm2conc(a$CO2, meta[i, ]$temp, pmbar)
      conc.CH4 <- ppm2conc(a$CH4, meta[i, ]$temp, pmbar)
      conc.N2O <- ppm2conc(a$N2O, meta[i, ]$temp, pmbar)

      # time in hour because algorithm expects that.
      # fluxes need to be trensformed to daily fluxes afterwards
      hmr_data_tmp <- data.frame(
        spot = meta[i, ]$spot,
        day = meta[i, ]$day, rep = repcount[repcount$spot ==
          meta[i, ]$spot, ]$count, V = V, A = A, Time =
          as.numeric(a$datetime - a[1, ]$datetime) / 60 / 60,
        CO2mmol = conc, CH4mmol = conc.CH4, N2Ommol = conc.N2O,
        ch4 = "ch4", co2 = "co2", n2o = "n2o"
      )
      hmr_data <- rbind(hmr_data, hmr_data_tmp)
    }
  }

  hmr_data <- hmr_data[-1, ]
}

#' Process GASMET data
#'
#' @param gasmet data frame; Recordings from GASMET.
#' @param meta data frame; Metainformations, see 'Details'.
#' @param V numeric; Chamber Volume. Default to 0.01461.
#' @param A numeric; Chamber Area. Default to 0.098.
#' @param wndw numeric; Default to 10. Number of datapoints per measurement.
#'   Can be overruled in metadata file. See 'Details'.
#' @param offset numeric; Default to 0. Number of datapoints skipped at the
#'   beginning. Can be overruled in metadata file. See 'Details'.
#' @param pre if TRUE don't process the flux calculation and return the
#'   preprocessed dataframe.
#'  When FALSE (the default) data is processed with routines from 'gasfluxes'
#'  package and the fluxes are returned.
#'
#' @return data frame with flux data in mmol m-2 d-1
#'
#' @details Flux calculation needs a GASMET datafile and a file containing
#'   metadata. Expected fields in the meta file are:
#'
#' \itemize{
#'   \item spot: character; ID of the measurement location.
#'   \item day: character; Date of the measurement.
#'   \item begin: character; Exact start of the measurement in the format
#'     HH:MM:SS. Ideally directly copied from the GASMET File.
#'   \item temp: number; Temperature inside the chamber during measurement.
#'   \item wndw: (optional) Number of datapoints per measurement. Function
#'     parameter is used if not provided (default: 10).
#'   \item offset: (optional) Number of datapoints skipped at the beginning.
#'     Function parameter is used if not provided (default: 0).}
#'
#'   The function splits the GASMET File into chunks containing the single
#'     chamber applications defined by begin, wndw, and offset. These chunks
#'     are passed to the \code{\link[gasfluxes]{gasfluxes}} package for the
#'     flux calculation and CO2, CH4 and N2O fluxes are returned as single data
#'     frame.
#'     \code{\link[gasfluxes]{gasfluxes}} creates a plot for every single
#'     measurement which are saved into a 'pics' subfolder within the working
#'     directory.
#'
#'     wndw and offset can be set as function parameters for the whole dataset
#'     or individually in the metadata file for every measurement.
#'
#'
#' @export
#' @examples
#' # preprocessing of data (pre=TRUE) returns data frame with chunks ready to
#'   # pass to 'gasfluxes'.
#' process_gasmet(gasmet, meta_gasmet, pre=TRUE)
#'
#' # prcessing provokes flux calculation for the three gas species
#'   # (CO2, CH4, N2O).
#' \dontrun{
#' process_gasmet(gasmet, meta_gasmet)
#' }
process_gasmet2 <- function(gasmet, meta, V = 0.01461, A = 0.098,
                           pre = F, wndw = 10, offset = 0) {
  # define begin to avoid check note for CRAN
  begin <- NULL

  hmr.data <- preprocess_gasmet(gasmet, meta,
    V = V, A = A, wndw = wndw,
    offset = offset
  )

  if (pre == TRUE) {
    return(hmr.data)
  }

  if (length(rownames(hmr.data)) == 0) {
    stop("No data to process.")
  }

  transect_flux_co2 <- gasfluxes::gasfluxes(hmr.data,
    .times = "Time",
    .C = "CO2mmol", .id = c("day", "co2", "spot", "rep"), methods =
      c("robust linear"), select = "RF2011"
  )
  transect_flux_ch4 <- gasfluxes::gasfluxes(hmr.data,
    .times =
      "Time", .C = "CH4mmol", .id = c("day", "ch4", "spot", "rep"), methods =
      c("robust linear"), select = "RF2011"
  )
  transect_flux_n2o <- gasfluxes::gasfluxes(hmr.data,
    .times = "Time",
    .C = "N2Ommol", .id = c("day", "n2o", "spot", "rep"), methods =
      c("robust linear"), select = "RF2011"
  )

  # flux in mmol m-2 d-1
  flux <- data.frame(
    date = as.POSIXct(transect_flux_co2$day,
      format =
        "%d.%m.%Y"
    ), site = transect_flux_co2$spot, CO2_flux =
      transect_flux_co2$robust.linear.f0 * 24, CO2_flux_p =
      transect_flux_co2$robust.linear.f0.p, CH4_flux =
      transect_flux_ch4$robust.linear.f0 * 24, CH4_flux_p =
      transect_flux_ch4$robust.linear.f0.p, N2O_flux =
      transect_flux_n2o$robust.linear.f0 * 24, N2O_flux_p =
      transect_flux_n2o$robust.linear.f0.p, begin =
      meta %>% dplyr::filter(!is.na(begin)) %>% dplyr::select(begin)
  )

}

preprocess_losgatos2 <- function(losgatos, meta, V = 0.01461, A = 0.098) {
  Time <- NULL
  meta$start <- lubridate::ceiling_date(meta$start, "minute",
    change_on_boundary = TRUE
  )
  meta$end <- lubridate::floor_date(meta$end, "minute")
  hmr_data <- dplyr::tibble(
    spot = NA, day = NA, rep = NA, V = NA, A = NA, Time = NA,
    CO2mmol = NA, CH4mmol = NA, co2 = NA, ch4 = NA
  )
  repcount <- data.frame(spot = unique(meta$spot), count = 0)
  for (i in 1:length(meta$spot)) {
    if (!is.na(meta[i, ]$start)) {
      repcount[repcount$spot == meta[i, ]$spot, ]$count <- repcount[repcount$spot == meta[i, ]$spot, ]$count + 1
      a <- losgatos %>%
        dplyr::filter(Time >= meta[i, ]$start & Time <= meta[i, ]$end)
      if (length(rownames(a)) == 0){
        warning(call. = FALSE, "meta entry", i, "skipped.",
          " No matching data.")
        next
      }
      pmbar <- mean(a$GasP_torr) * 1.33322
      temp <- mean(c(meta$t_start, meta$t_end), na.rm = TRUE)
      if (is.na(temp)){
        temp <- mean(a$AmbT_C, na.rm = TRUE)
      }

      # calculate concentration in mmol/m³
      conc     <- ppm2conc(a$`[CO2]_ppm`, temp, pmbar)
      conc.CH4 <- ppm2conc(a$`[CH4]_ppm`, temp, pmbar)

      hmr_data_tmp <- data.frame(
        spot = meta[i, ]$spot, day = as.character(meta[i, ]$day), rep =
          repcount[repcount$spot == meta[i, ]$spot, ]$count, V = V, A = A,
          Time = as.numeric(a$Time - a[1, ]$Time) / 60 / 60, CO2mmol = conc,
          CH4mmol = conc.CH4, ch4 = "ch4", co2 = "co2"
      )
      hmr_data <- rbind(hmr_data, hmr_data_tmp)
    }
  }
  hmr_data <- hmr_data[-1, ]
}

#' Process flux calculation for LosGatos data.
#'
#' @param losgatos a data frame containing LosGatos data.
#' @param meta a data frame of meta information. See 'Details'.
#' @param V numeric; Chamber volume.
#' @param A numeric; Chamber area.
#' @param pre if TRUE don't process the flux calculation and return the
#'   preprocessed dataframe. When FALSE (the default) data is processed with
#'   routines from 'gasfluxes' package and the fluxes are returned.
#'
#' @return Fluxes
#' @export
#'
process_losgatos2 <- function(losgatos, meta, V = 0.01461, A = 0.098, pre = F) {
  hmr_data <- preprocess_losgatos(losgatos, meta, V = V, A = A)

  # define begin to avoid check note for CRAN
  start <- NULL

  if (pre == TRUE) {
    return(hmr_data)
  }

  transect_flux_co2 <- gasfluxes::gasfluxes(hmr_data,
    .times = "Time", .C =
      "CO2mmol", .id = c("day", "co2", "rep", "spot"), methods =
      c("robust linear"), select = "RF2011"
  )
  transect_flux_ch4 <- gasfluxes::gasfluxes(hmr_data,
    .times = "Time", .C =
      "CH4mmol", .id = c("day", "ch4", "rep", "spot"), methods =
      c("robust linear"), select = "RF2011"
  )

  # flux in mmol m-2 d-1
  flux <- data.frame(
    date = lubridate::ymd(transect_flux_co2$day),
      site = transect_flux_co2$spot, CO2_flux =
      transect_flux_co2$robust.linear.f0 * 24, CO2_flux_p =
      transect_flux_co2$robust.linear.f0.p, CH4_flux =
      transect_flux_ch4$robust.linear.f0 * 24, CH4_flux_p =
      transect_flux_ch4$robust.linear.f0.p, begin =
      meta %>% dplyr::filter(!is.na(start)) %>% dplyr::select(start)
  )

}

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
#' @param conc Data frame. Recorded data from device.
#' @param meta Data frame. Metadata containing required informations.
#' @param time_stamp Character vector. Value will be used as the column in conc
#'   containing the timestamp.
#' @param conc_columns A named character vector. Values will be used to extract
#'   ppm data from conc. E.g. c(CO2 = "CO2").
#' @param preassure Character.
#' @param preassure_factor Numeric value to convert preassure to mbar.
#' @param temperature Character.
#' @param manuel_temperature Character.
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
#' @param pre If FALSE, flux processing will be executed. If FALSE flux
#'   processing will be skipped and preprocessed data frame will be returned.
#'
#' @return A data frame.
#' @export
#'
process_chamber <- function(conc, meta, time_stamp, conc_columns, preassure,
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
  hmr_data <- preprocess_chamber(conc, meta, device, V, A)

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
                           start = "start", end = "wndw", V, A, pre = FALSE){
  device <- device_gasmet(manual_temperature = manual_temperature, spot = spot, day = day,
    start = start, end = end)
  hmr_data <- preprocess_chamber(data, meta, device, V, A)

  if (pre == TRUE) {
    return(hmr_data)
  }

  flux <- process_flux(hmr_data, meta, device)
  return(flux)
}
