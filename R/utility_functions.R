
#' Calculate Schmidt Number
#'
#' @param wt numeric; Water temperature.
#' @param gas character; Gas to calculate SN for. See 'Details'.
#'
#' @return
#' @export
#'
#' @examples
calc.SN <- function(wt, gas = "co2") {
    switch(tolower(gas), co2 = {
        SN <- 1923.6 + (-125.06 * wt) + (4.3773 * wt^2) + (-0.085681 * wt^3) + (0.00070284 * wt^4)
    }, ch4 = {
        SN <- 1909.4 + (-120.78 * wt) + (4.1555 * wt^2) + (-0.080578 * wt^3) + (0.00065777 * wt^4)
    })
}

calc.u10 <- function(ws) {
    u10 <- (1.22 * ws)
    # u10 <- ws * 10^0.16
    return(u10)
}

#' calc.kW
#'
#' @param ws numeric; windspeed m/s
#' @param wt numeric; water temperature °C
#' @param gas character; one of 'CO2', 'CH4'
#' @param model character; model to calculate
#'
#' @return kW for the given parameters
#' @export
#'
#' @examples
calc.kW <- function(ws, wt, gas = "co2", model = "wann14") {
    switch(tolower(model), wann14 = {
        kW <- switch(tolower(gas), co2 = {
            0.251 * calc.u10(ws)^2 * (calc.SN(wt, gas)/660)^-0.5
        }, ch4 = {
            0.251 * calc.u10(ws)^2 * (calc.SN(wt, gas)/660)^-0.5
        })
    })
    return(kW)
}
