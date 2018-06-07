#' Label axis for CO2 according to flux scale.
#'
#' @param time character; Timescale. See 'Details'.
#'
#' @return ggplot ylab object.
#' @export
#'
axis.flux.co2 <- function(time) {
    time <- tolower(time)
    supportedTimes <- c("sec", "day")
    if (!time %in% supportedTimes) {
        stop("Time not supported")
    }
    axis <- switch(time, sec = ggplot2::ylab(expression(CO[2] ~ Flux ~ "\U00B5"~mol ~ m^-2 ~ s^-1)), day = ggplot2::ylab(expression(CO[2] ~
        Flux ~ mmol ~ m^-2 ~ d^-1)))
}

#' Label axis for arbitrary gases according to flux scale.
#'
#' @inheritParams axis.flux.co2
#' @export
axis.flux.gas <- function(time) {
    time <- tolower(time)
    supportedTimes <- c("sec", "day")
    if (!time %in% supportedTimes) {
        stop("Time not supported")
    }
    axis <- switch(time, sec = ggplot2::ylab(expression(Gas ~ Flux ~ "\U00B5"~mol ~ m^-2 ~ s^-1)), day = ggplot2::ylab(expression(Gas ~
        Flux ~ mmol ~ m^-2 ~ d^-1)))
}


#' Label axis for CH4 according to flux scale.
#'
#' @inheritParams axis.flux.co2
#' @export
axis.flux.ch4 <- function(time) {
    time <- tolower(time)
    supportedTimes <- c("sec", "day")
    if (!time %in% supportedTimes) {
        stop("Time not supported")
    }
    axis <- switch(time, sec = ggplot2::ylab(expression(CH[4] ~ Flux ~ "\U00B5"~mol ~ m^-2 ~ s^-1)), day = ggplot2::ylab(expression(CH[4] ~
        Flux ~ mmol ~ m^-2 ~ d^-1)))
}
