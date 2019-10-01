#' Label ggplot2 axis according to temporal scale of gasflux
#'
#'
#'
#' @param time character; Either "sec" oder "day". See 'Details'.
#'
#' @return Returns \code{\link[ggplot2]{ylab}} object
#' @details "sec" returns expression(## ~ flux ~ ("µ" ~ mol ~ m^-2 ~ s^-1))),
#'
#'     "day" returns expression(## ~ flux ~ (mmol ~ m^-2 ~ d^-1))),
#'
#'     where ## is either "Gas", "\out{CO<sub>2</sub>}", or "\out{CH<sub>4</sub>}"
#' @export
#' @examples
#' library(ggplot2)
#'
#' df <- data.frame(
#'     plot = rep(c("A", "B", "C"), each = 20),
#'     flux = c(rnorm(20), rnorm(20), rnorm(20))
#' )
#'
#' ggplot(df, aes(plot, flux))+
#' geom_boxplot()+
#' axis.flux.gas("day")
#'
axis.flux.co2 <- function(time) {
  time <- tolower(time)
  supported_times <- c("sec", "day")
  if (!time %in% supported_times) {
    stop("Time not supported")
  }
  axis <- switch(time,
    sec = ggplot2::ylab(expression(CO[2] ~ flux ~ ("\u00B5" ~ mol ~ m^-2 ~ s^-1))),
    day = ggplot2::ylab(expression(CO[2] ~ flux ~ (mmol ~ m^-2 ~ d^-1)))
  )
}

#' @rdname axis.flux.co2
#' @export
axis.flux.gas <- function(time) {
  time <- tolower(time)
  supported_times <- c("sec", "day")
  if (!time %in% supported_times) {
    stop("Time not supported")
  }
  axis <- switch(time,
    sec = ggplot2::ylab(expression(Gas ~ flux ~ ("\u00B5" ~ mol ~ m^-2 ~ s^-1))),
    day = ggplot2::ylab(expression(Gas ~ flux ~ (mmol ~ m^-2 ~ d^-1)))
  )
}


#' @rdname axis.flux.co2
#' @export
axis.flux.ch4 <- function(time) {
  time <- tolower(time)
  supported_times <- c("sec", "day")
  if (!time %in% supported_times) {
    stop("Time not supported")
  }
  axis <- switch(time,
    sec = ggplot2::ylab(expression(CH[4] ~ flux ~ ("\u00B5" ~ mol ~ m^-2 ~ s^-1))),
    day = ggplot2::ylab(expression(CH[4] ~ flux ~ (mmol ~ m^-2 ~ d^-1)))
  )
}
