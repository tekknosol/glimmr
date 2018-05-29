
#' Calculate Schmidt Number
#'
#' @param wt numeric; Water temperature.
#' @param gas character; Gas to calculate SN for. See 'Details'.
#'
#' @return test
#' @export
#'
calc_SN <- function(wt, gas = "co2") {
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

#' Calculate Gas transfer coefficient
#'
#' @param ws numeric; windspeed m/s
#' @param wt numeric; water temperature °C
#' @param gas character; one of 'CO2', 'CH4'
#' @param model character; model to calculate kW. See 'Details'
#'
#' @details Currently these models are supported:
#'
#'   WANN14: Wanninkhof (2014)
#'
#' @return kW for the given parameters
#' @references Wanninkhof, R., 2014. Relationship between wind speed and gas exchange over the ocean revisited. Limnology and Oceanography: Methods 12, 351–362. https://doi.org/10.4319/lom.2014.12.351
#' @export
#'
calc_kW <- function(ws, wt, gas = "co2", model = "wann14") {
    switch(tolower(model), wann14 = {
        kW <- switch(tolower(gas), co2 = {
            0.251 * calc.u10(ws)^2 * (calc.SN(wt, gas)/660)^-0.5
        }, ch4 = {
            0.251 * calc.u10(ws)^2 * (calc.SN(wt, gas)/660)^-0.5
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
  })
  return(kW)
}

