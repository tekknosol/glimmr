
#' Calculate Schmidt Number
#'
#' @param wt numeric; Water temperature.
#' @param gas character; Gas to calculate SN for. See 'Details'.
#'
#' @return test
#' @examples
#' watertemperature <- rnorm(5, mean = 20)
#'
#' calc_SN(watertemperature)
#'
#' calc_SN(watertemperature, "ch4")
#' @export
#'
calc_SN <- function(wt, gas = "CO2") {
  supported_gases <- c("co2", "ch4")
  gas <- match.arg(tolower(gas), supported_gases)
  switch(gas,
    co2 = {
      SN <- 1923.6 + (-125.06 * wt) + (4.3773 * wt^2) + (-0.085681 * wt^3) + (0.00070284 * wt^4)
    },
    ch4 = {
      SN <- 1909.4 + (-120.78 * wt) + (4.1555 * wt^2) + (-0.080578 * wt^3) + (0.00065777 * wt^4)
    }
  )
  return(SN)
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

#' Returns the gas transfer velocity (k600)
#'
#' @param ws numeric; Windspeed in m/s
#'
#' @return Numeric value of gas exchange velocity (k600). Should be converted to
#' desired gas with \code{\link{calc_kW}}
calc_k600 <- function(ws) {
  # cole98
  k600 <- 0.215 * calc_u10(ws)^1.7 + 2.07
  # #wann92 k600 <- 0.228 * calc.u10(ws)^2.2 + 0.168
  return(k600)
}

#' Returns the gas transfer coefficient for gas of interest based on different
#' models.
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
#' @return Numeric value of gas transfer coefficient for the given parameters
#' @references Wanninkhof, R., 2014. Relationship between wind speed and gas
#' exchange over the ocean revisited. Limnology and Oceanography: Methods 12,
#' 351–362. https://doi.org/10.4319/lom.2014.12.351
#'
#'   Cole, J.J., Caraco, N.F., 1998. Atmospheric exchange of carbon dioxide in
#'   a low-wind oligotrophic lake measured by the addition of SF6. Limnology
#'   and Oceanography 43, 647–656. https://doi.org/10.4319/lo.1998.43.4.0647
#' @examples
#' # Windpseed in m/s
#' windspeed <- c(2, 4.3, 1.8)
#' # Water temperature in °C
#' watertemperature <- c(18, 21, 19.3)
#'
#' # Calculate gas transfer coefficient
#' calc_kW(windspeed, watertemperature)
#'
#' @export
#'
calc_kW <- function(ws, wt, gas = "CO2", model = "wann14") {
  supported_gases <- c("co2", "ch4")
  supported_models <- c("wann14", "cole98")
  gas <- match.arg(tolower(gas), supported_gases)
  model <- match.arg(tolower(model), supported_models)

  switch(model,
    wann14 = {
    kW <- switch(tolower(gas),
      co2 = {
        0.251 * calc_u10(ws)^2 * (calc_SN(wt, gas) / 660)^-0.5
      },
      ch4 = {
        0.251 * calc_u10(ws)^2 * (calc_SN(wt, gas) / 660)^-0.5
      })
    },
    cole98 = {
      # ifelse(is.na(ws), return(NA), n <- 0.67)
      ifelse(ws > 3, n <- 0.5, n <- 0.67)
      kW <- switch(tolower(gas),
        co2 = {
          calc_k600(ws) * (600 / calc_SN(wt, gas))^n
        },
        ch4 = {
          calc_k600(ws) * (600 / calc_SN(wt, gas))^n
        })
    })
  return(kW)
}

#' @inheritParams calc_kW
calc.kW <- function(ws, wt, gas = "co2", model = "wann14") {
  switch(tolower(model), wann14 = {
    kW <- switch(tolower(gas), co2 = {
      0.251 * calc_u10(ws)^2 * (calc_SN(wt, gas) / 660)^-0.5
    }, ch4 = {
      0.251 * calc_u10(ws)^2 * (calc_SN(wt, gas) / 660)^-0.5
    })
  }, cole98 = {
    ifelse(is.na(ws), return(NA), n <- 0.67)
    ifelse(ws > 3, n <- 0.5, n <- 0.67)
    kW <- switch(tolower(gas), co2 = {
      calc_k600(ws) * (600 / calc_SN(wt, gas))^n
    }, ch4 = {
      calc_k600(ws) * (600 / calc_SN(wt, gas))^n
    })
  })
  return(kW)
}

#' Returns Henry's constant for gas of interest
#'
#' @param t numeric; temperature in °C
#' @param gas character; Gas to calculate kH for. See 'Details'.
#'
#' @details Currently supported gases:
#'
#'  \itemize{
#'    \item CO2
#'    \item CH4
#'  }
#'
#' @return Numeric value of Henry's constant \out{H<sup>cp</sup>} (mol/(L*atm))
#'
#' @examples
#' watertemperature <- rnorm(5, mean = 20)
#'
#' calc_kH(watertemperature)
#'
#' calc_kH(watertemperature, "ch4")
#'
#' @export
#'
calc_kH <- function(t, gas = "CO2") {
  supported_gases <- c("co2", "ch4")
  gas <- match.arg(tolower(gas), supported_gases)
  kH <- switch(tolower(gas), co2 = 0.034 * exp(2400 * (1 / (t + 273.15) - 1 / 298.15)), ch4 = 0.0013 *
    exp(1600 * (1 / (t + 273.15) - 1 / 298.15)))
  return(kH)
}

#' Performs unit conversion for gas fluxes
#'
#' @param flux numeric; Flux value in mmol/sqm/d or µMol/sqm/d
#'
#' @return Numeric value of converted flux.
#' @export
#'
day2sec <- function(flux) {
  return(flux * 1000 / 86400)
}


#' @rdname day2sec
#' @export
#'
sec2day <- function(flux) {
  return(flux * 86400 / 1000)
}
