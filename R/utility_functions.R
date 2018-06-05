
#' Calculate Schmidt Number
#'
#' @param wt numeric; Water temperature.
#' @param gas character; Gas to calculate SN for. See 'Details'.
#'
#' @return test
#' @export
#'
calc_SN <- function(wt, gas = "co2") {
    supportedGases <- c("co2", "ch4")
    gas <- tolower(gas)
    if (!gas %in% supportedGases) {
        stop("gas not supported")
    }
    switch(tolower(gas), co2 = {
        SN <- 1923.6 + (-125.06 * wt) + (4.3773 * wt^2) + (-0.085681 * wt^3) + (0.00070284 * wt^4)
    }, ch4 = {
        SN <- 1909.4 + (-120.78 * wt) + (4.1555 * wt^2) + (-0.080578 * wt^3) + (0.00065777 * wt^4)
    })
}

#' @inheritParams calc_SN
calc.SN <- function(wt, gas = "co2") {
    .Deprecated("calc_SN")
    switch(tolower(gas), co2 = {
        SN <- 1923.6 + (-125.06 * wt) + (4.3773 * wt^2) + (-0.085681 * wt^3) + (0.00070284 * wt^4)
    }, ch4 = {
        SN <- 1909.4 + (-120.78 * wt) + (4.1555 * wt^2) + (-0.080578 * wt^3) + (0.00065777 * wt^4)
    })
}

calc_u10 <- function(ws) {
    u10 <- (1.22 * ws)
    # u10 <- ws * 10^0.16
    return(u10)
}

calc.u10 <- function(ws) {
    .Deprecated("calc_u10")
    u10 <- (1.22 * ws)
    # u10 <- ws * 10^0.16
    return(u10)
}

#' Calculate k600
#'
#' @param ws numeric; Windspeed m/s
#'
#' @return k600 based on windspeed
calc_k600 <- function(ws) {
    # cole98
    k600 <- 0.215 * calc_u10(ws)^1.7 + 2.07
    # #wann92 k600 <- 0.228 * calc.u10(ws)^2.2 + 0.168
    return(k600)
}

#' Calculate Gas transfer coefficient
#'
#' @param ws numeric; windspeed m/s
#' @param wt numeric; water temperature °C
#' @param gas character; one of 'CO2', 'CH4'
#' @param model character; model to calculate kW. See 'Details'
#'
#' @details Currently these models are supported:
#'
#'  \itemize{
#'    \item WANN14: Wanninkhof (2014)
#'    \item COLE98: Cole and Caraco (1998)
#'  }
#'
#'
#' @return kW for the given parameters
#' @references Wanninkhof, R., 2014. Relationship between wind speed and gas exchange over the ocean revisited. Limnology and Oceanography: Methods 12, 351–362. https://doi.org/10.4319/lom.2014.12.351
#'
#'   Cole, J.J., Caraco, N.F., 1998. Atmospheric exchange of carbon dioxide in a low-wind oligotrophic lake measured by the addition of SF6. Limnology and Oceanography 43, 647–656. https://doi.org/10.4319/lo.1998.43.4.0647
#' @export
#'
calc_kW <- function(ws, wt, gas = "co2", model = "wann14") {
    gas <- tolower(gas)
    model <- tolower(model)
    supportedGases <- c("co2", "ch4")
    supportedModels <- c("wann14", "cole98")
    if (!gas %in% supportedGases) {
        stop("gas not supported")
    }
    if (!model %in% supportedModels) {
        stop("model not supported")
    }
    switch(tolower(model), wann14 = {
        kW <- switch(tolower(gas), co2 = {
            0.251 * calc.u10(ws)^2 * (calc.SN(wt, gas)/660)^-0.5
        }, ch4 = {
            0.251 * calc.u10(ws)^2 * (calc.SN(wt, gas)/660)^-0.5
        })
    }, cole98 = {
        ifelse(is.na(ws), return(NA), n <- 0.67)
        ifelse(ws > 3, n <- 0.5, n <- 0.67)
        kW <- switch(tolower(gas), co2 = {
            calc_k600(ws) * (600/calc_SN(wt, gas))^n
        }, ch4 = {
            calc_k600(ws) * (600/calc_SN(wt, gas))^n
        })
    })
    return(kW)
}

#' @inheritParams calc_kW
calc.kW <- function(ws, wt, gas = "co2", model = "wann14") {
    switch(tolower(model), wann14 = {
        kW <- switch(tolower(gas), co2 = {
            0.251 * calc.u10(ws)^2 * (calc.SN(wt, gas)/660)^-0.5
        }, ch4 = {
            0.251 * calc.u10(ws)^2 * (calc.SN(wt, gas)/660)^-0.5
        })
    }, cole98 = {
        ifelse(is.na(ws), return(NA), n <- 0.67)
        ifelse(ws > 3, n <- 0.5, n <- 0.67)
        kW <- switch(tolower(gas), co2 = {
            calc_k600(ws) * (600/calc_SN(wt, gas))^n
        }, ch4 = {
            calc_k600(ws) * (600/calc_SN(wt, gas))^n
        })
    })
    return(kW)
}

#' Calculate Henry's constant
#'
#' @param t numeric; temperature °C
#' @param gas character; Gas to calculate kH for. See 'Details'.
#'
#' @details Currently these gases are supported:
#'
#'  \itemize{
#'    \item CO_2
#'    \item CH_4
#'  }
#'
#' @return Henry's constant
#' @export
#'
kH <- function(t, gas = "co2") {
    supportedGases <- c("co2", "ch4")
    gas <- tolower(gas)
    if (!gas %in% supportedGases) {
        stop("gas not supported")
    }
    kH <- switch(tolower(gas), co2 = 0.034 * exp(2400 * (1/(t + 273.15) - 1/298.15)), ch4 = 0.0013 * 
        exp(1600 * (1/(t + 273.15) - 1/298.15)))
    return(kH)
}

#' Convert mmol m⁻² d⁻¹ to µMol m⁻² d⁻¹
#'
#' @param flux numeric; Flux value in mmol m⁻² d⁻¹
#'
#' @return Flux in µMol m⁻² d⁻¹
#' @export
#'
day2sec <- function(flux) {
    return(flux * 1000/86400)
}

#' Converts µMol m⁻² d⁻¹ to mmol m⁻² d⁻¹
#'
#' @param flux numeric; Flux value in µMol m⁻² d⁻¹
#'
#' @return Flux in mmol m⁻² d⁻¹
#' @export
#'
sec2day <- function(flux) {
    return(flux * 86400/1000)
}
