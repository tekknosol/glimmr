#' Preprocess GASMET data for flux calculations
#'
#' @param gasmet data frame; Recordings from GASMET.
#' @param meta data frame; Metainformations, see 'Details'.
#' @param V numeric; Chamber Volume. Default to 0.01461.
#' @param A numeric; Chamber Area. Default to 0.098.
#'
#' @return dataframe which can be used as input of the gasfluxes function
#' @export
#'
#' @examples
#' flux_raw <- preprocess_gasmet(gasmet, meta)
preprocess_gasmet <- function(gasmet, meta, V = 0.01461, A = 0.098) {
    hmr.data <- data.frame(spot = NA, day = NA, rep = NA, V = NA, A = NA, Time = NA, Concentration = NA, CH4mmol = NA,
        co2 = NA, ch4 = NA)
    for (i in 1:length(meta$spot)) {
        if (!is.na(meta[i, ]$begin)) {
            series <- paste(rownames(meta[i, ]), meta[i, ]$spot, sep = "-")
            begin <- which(gasmet$datetime == meta[i, ]$begin)
            a <- gasmet[(begin + meta[i, ]$offset):(begin + meta[i, ]$wndw - 1), ]
            conc <- a$CO2 * 10^-6 * (meta[i, ]$pmbar * 100)/(8.314 * (meta[i, ]$temp + 273.15)) * 1000  # calculate concentration in mmol/m³
            conc.CH4 <- a$CH4 * 10^-6 * (meta[i, ]$pmbar * 100)/(8.314 * (meta[i, ]$temp + 273.15)) * 1000

            # time in hour because algorithm expects that. fluxes need to be trensformed to daily fluxes afterwards
            hmr.data.tmp <- data.frame(spot = meta[i, ]$spot, day = meta[i, ]$day, rep = rownames(meta[i, ]),
                V = V, A = A, Time = as.numeric(a$datetime - a[1, ]$datetime)/60/60, Concentration = conc, CH4mmol = conc.CH4,
                ch4 = "ch4", co2 = "co2")
            hmr.data <- rbind(hmr.data, hmr.data.tmp)
        }
    }
    hmr.data <- hmr.data[-1, ]
    return(hmr.data)
}
