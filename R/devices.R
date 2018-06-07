#' Preprocess GASMET data for flux calculations
#'
#' @param gasmet data frame; Recordings from GASMET.
#' @param meta data frame; Metainformations, see 'Details'.
#' @param V numeric; Chamber Volume. Default to 0.01461.
#' @param A numeric; Chamber Area. Default to 0.098.
#'
#'
#' @return dataframe which can be used as input of the gasfluxes function
preprocess_gasmet <- function(gasmet, meta, V = 0.01461, A = 0.098) {
    terminate <- F
    if (T %in% is.na(meta$temp)) {
        terminate <- T
    }
    if (terminate) {
        stop("missing temperature values in meta file. Flux calculation stoped.")
    }

    if (T %in% is.na(meta$wndw) | is.null(meta$wndw)) {
        meta$wndw <- 10
        warning("No window provided in meta file. Set to 10.")
    }

    if (T %in% is.na(meta$offset) | is.null(meta$offset)) {
        meta$offset <- 0
        warning("No offset provided in meta file. Set to 0.")
    }

    hmr.data <- data.frame(spot = NA, day = NA, rep = NA, V = NA, A = NA, Time = NA, CO2mmol = NA,
        CH4mmol = NA, N2Ommol = NA, co2 = NA, ch4 = NA, n2o = NA)
    for (i in 1:length(meta$spot)) {
        if (!is.na(meta[i, ]$begin)) {
            series <- paste(rownames(meta[i, ]), meta[i, ]$spot, sep = "-")
            begin <- which(gasmet$datetime == meta[i, ]$begin)
            a <- gasmet[(begin + meta[i, ]$offset):(begin + meta[i, ]$wndw - 1), ]
            pmbar <- mean(a$Luftdruck)
            conc <- a$CO2 * 10^-6 * (pmbar * 100)/(8.314 * (meta[i, ]$temp + 273.15)) * 1000  # calculate concentration in mmol/m³
            conc.CH4 <- a$CH4 * 10^-6 * (pmbar * 100)/(8.314 * (meta[i, ]$temp + 273.15)) * 1000
            conc.N2O <- a$N2O * 10^-6 * (pmbar * 100)/(8.314 * (meta[i, ]$temp + 273.15)) * 1000

            # time in hour because algorithm expects that. fluxes need to be trensformed to daily fluxes
            # afterwards
            hmr.data.tmp <- data.frame(spot = meta[i, ]$spot, day = meta[i, ]$day, rep = rownames(meta[i,
                ]), V = V, A = A, Time = as.numeric(a$datetime - a[1, ]$datetime)/60/60, CO2mmol = conc,
                CH4mmol = conc.CH4, N2Ommol = conc.N2O, ch4 = "ch4", co2 = "co2", n2o = "n2o")
            hmr.data <- rbind(hmr.data, hmr.data.tmp)
        }
    }
    hmr.data <- hmr.data[-1, ]

    return(hmr.data)
}

#' Process GASMET data
#'
#' @inheritParams preprocess_gasmet
#' @param pre if TRUE don't process the flux calculation and return the preprocessed dataframe.
#'     When FALSE (the default) data is processed with routines from 'gasfluxes' package and the fluxes are returned.
#'
#' @return data frame with flux data
#'
#' @details Flux calculation needs a GASMET datafile and a file containing metadata. Excpected fields in the
#'    meta file are:
#'
#' \itemize{
#'   \item spot: character; ID of the measurement location.
#'   \item day: character; Date of the measurement.
#'   \item begin: character; Exact beginning of the measurement. Ideally directly copied from the GASMET File.
#'   \item temp: number; Temperature during measurement.
#'   \item wndw: (optional) Number of datapoints per measurement. Set to 10 if not provided.
#'   \item offset: (optional) Number of datapoints skipped at the beginning. Set to 0 if not provided.}
#'
#'   The function splits the GASMET File into chunks containing the single chamber applications defined by
#'     begin, wndw, and offset. These chunks are passed to the 'gasfluxes' package for the flux calculation.
#'
#'
#' @export
#'
process_gasmet <- function(gasmet, meta, V = 0.01461, A = 0.098, pre = F) {
    hmr.data <- preprocess_gasmet(gasmet, meta)

    if (pre == T) {
        return(hmr.data)
    }

    transect.flux.co2 <- gasfluxes::gasfluxes(hmr.data, .times = "Time", .C = "CO2mmol", .id = c("day",
        "co2", "rep", "spot"), methods = c("robust linear"), select = "RF2011")
    transect.flux.ch4 <- gasfluxes::gasfluxes(hmr.data, .times = "Time", .C = "CH4mmol", .id = c("day",
        "ch4", "rep", "spot"), methods = c("robust linear"), select = "RF2011")
    transect.flux.n2o <- gasfluxes::gasfluxes(hmr.data, .times = "Time", .C = "N2Ommol", .id = c("day",
        "n2o", "rep", "spot"), methods = c("robust linear"), select = "RF2011")

    # flux in mmol m-2 d-1
    flux <- data.frame(date = as.POSIXct(transect.flux.co2$day, format = "%d.%m.%Y"), site = transect.flux.co2$spot,
        CO2.flux = transect.flux.co2$robust.linear.f0 * 24, CO2.flux.p = transect.flux.co2$robust.linear.f0.p,
        CH4.flux = transect.flux.ch4$robust.linear.f0 * 24, CH4.flux.p = transect.flux.ch4$robust.linear.f0.p,
        N2O.flux = transect.flux.n2o$robust.linear.f0 * 24, N2O.flux.p = transect.flux.n2o$robust.linear.f0.p,
        begin = meta %>% dplyr::filter(!is.na(begin)) %>% dplyr::select(begin))

    return(flux)
}

preprocess_losgatos <- function(losgatos, meta, V = 0.01461, A = 0.098) {
    meta$start <- lubridate::ceiling_date(meta$start, "minute", change_on_boundary = T)
    meta$end <- lubridate::floor_date(meta$end, "minute")
    hmr.data <- data.frame(spot = NA, rep = NA, V = NA, A = NA, Time = NA, CO2mmol = NA, CH4mmol = NA,
        co2 = NA, ch4 = NA)
    for (i in 1:length(meta$spot)) {
        if (!is.na(meta[i, ]$start)) {
            begin <- which(losgatos$Time >= meta[i, ]$start)[1]
            end <- which(losgatos$Time <= meta[i, ]$end)
            end <- end[length(end)]
            a <- losgatos[begin:end, ]
            pmbar <- mean(a$GasP_torr) * 1.33322
            temp <- mean(c(meta$tstart, meta$tend))
            conc <- a$`[CO2]_ppm` * 10^-6 * (pmbar * 100)/(8.314 * (temp + 273.15)) * 1000  # calculate concentration in mmol/m³
            conc.CH4 <- a$`[CH4]_ppm` * 10^-6 * (pmbar * 100)/(8.314 * (temp + 273.15)) * 1000
            hmr.data.tmp <- data.frame(spot = meta[i, ]$spot, rep = rownames(meta[i, ]), V = V,
                A = A, Time = as.numeric(a$Time - a[1, ]$Time)/60/60, CO2mmol = conc, CH4mmol = conc.CH4,
                ch4 = "ch4", co2 = "co2")
            hmr.data <- rbind(hmr.data, hmr.data.tmp)
        }
    }
    hmr.data <- hmr.data[-1, ]
    return(hmr.data)
}

#' Process flux calculation for LosGatos data.
#'
#' @param losgatos a data frame containing LosGatos data.
#' @param meta a data frame of meta information. See 'Details'.
#' @param V numeric; Chamber volume.
#' @param A numeric; Chamber area.
#' @param pre if TRUE don't process the flux calculation and return the preprocessed dataframe.
#'     When FALSE (the default) data is processed with routines from 'gasfluxes' package and the fluxes are returned.
#'
#' @return Fluxes
#' @export
#'
process_losgatos <- function(losgatos, meta, V = 0.01461, A = 0.098, pre = F) {
    hmr.data <- preprocess_losgatos(losgatos, meta, V = V, A = A)

    if (pre == T) {
        return(hmr.data)
    }

    transect.flux.co2 <- gasfluxes::gasfluxes(hmr.data, .times = "Time", .C = "CO2mmol", .id = c("day",
        "co2", "rep", "spot"), methods = c("robust linear"), select = "RF2011")
    transect.flux.ch4 <- gasfluxes::gasfluxes(hmr.data, .times = "Time", .C = "CH4mmol", .id = c("day",
        "ch4", "rep", "spot"), methods = c("robust linear"), select = "RF2011")

    # flux in mmol m-2 d-1
    flux <- data.frame(date = as.POSIXct(transect.flux.co2$day, format = "%d.%m.%Y"), site = transect.flux.co2$spot,
        CO2.flux = transect.flux.co2$robust.linear.f0 * 24, CO2.flux.p = transect.flux.co2$robust.linear.f0.p,
        CH4.flux = transect.flux.ch4$robust.linear.f0 * 24, CH4.flux.p = transect.flux.ch4$robust.linear.f0.p,
        begin = meta %>% dplyr::filter(!is.na(begin)) %>% dplyr::select(begin))

    return(flux)
}
