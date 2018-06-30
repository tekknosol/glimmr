.all_gals_params <- c("name", "fixed_params", "time_stamp", "conc_columns",
  "preassure", "preassure_factor", "temperature", "manual_temperature",
  "offset", "duration_count", "spot", "day", "start", "end", "trimmer", "V", "A")

gals_default_vals <- c(name = "custom", fixed_params = c(), time_stamp = NULL,
                      conc_columns = NULL, preassure = NULL, preassure_factor = 1,
                      temperature = NULL, manual_temperature = NULL, offset = 0, duration_count = FALSE,
                      spot = "spot", day = "day", start = "start", end = "end",
                      trimmer = NULL, V = 0.01461, A = 0.098)


#' Gas analyzer setup
#'
#'
#' @param ... List of name value pairs to setup the gasanalyzer.
#' @param .default If TRUE, default settings are used in addition to the passed
#'   ones. Useful to abbreviate the function call.
#'
#' @eval rd_gals("default")
#' @return A gals object.
#' @export
gals <- function(..., .default = FALSE){
  exprs <- rlang::enquos(...)
  exprs <- lapply(exprs, function(x){
    if(rlang::quo_is_call(x)){ x <- eval(rlang::quo_get_expr(x)) }
    else{x <- rlang::quo_get_expr(x)}
    x
  })
  x <- structure(
    exprs,
    class = "gals"
  )
  if (.default) x <- set_gals_default(x)
  x
}

gals_default <- function(){
  gals(
    name = "custom", fixed_params = c(), time_stamp = NULL,
    conc_columns = NULL, preassure = NULL, preassure_factor = 1,
    temperature = NULL, manual_temperature = NULL, offset = 0, duration_count = FALSE,
    spot = "spot", day = "day", start = "start", end = "end",
    trimmer = NULL, V = 0.01461, A = 0.098
  )
}

set_gals_default <- function(x){
  x <- update(gals_default(), x)
  x <- structure(x[order(match(names(x),.all_gals_params))], class="gals")
  x
}


#' LosGatos setup
#'
#' Explicitly create LosGatos setup. Usually you will not call this
#' function, but will instead use the wrapper [process_losgatos()]
#'
#' @export
#'
#'
#' @eval rd_gals("losgatos")
gals_losgatos <- function(.default = TRUE){
  lg <- gals(
    fixed_params = c("name", "time_stamp", "conc_columns", "preassure", "preassure_factor", "temperature"),
    name = "losgatos",
    time_stamp = "Time",
    conc_columns = c(CH4 = "[CH4]_ppm", CO2 = "[CO2]_ppm"),
    preassure = "GasP_torr",
    preassure_factor = 1.33322,
    temperature = "AmbT_C",
    trimmer = trim_time
  )

  if (.default){
    lg <- set_gals_default(lg)
  }
  lg
}

#' GASMET setup
#'
#' Explicitly create LosGatos setup. Usually you will not call this
#'   function, but will instead use the wrapper [process_gasmet()]
#'
#' @export
#'
#'
#' @eval rd_gals("gasmet")
gals_gasmet <- function(.default = TRUE){
  lg <- gals(
    fixed_params = c("name", "time_stamp", "conc_columns", "preassure", "temperature", "preassure_factor", "duration_count"),
    name = "gasmet",
    time_stamp = "datetime",
    conc_columns = c(CH4 = "CH4", CO2 = "CO2", N2O = "N2O"),
    preassure = "Luftdruck",
    temperature = NA,
    manual_temperature = "temp",
    duration_count = TRUE,
    end = "wndw"
  )

  if (.default){
    lg <- update(gals_default(), lg)
  }
  lg <- structure(lg[order(match(names(lg),.all_gals_params))], class="gals")
  lg
}

update.gals <- function(gal, gal2){
  locked <- names(gal)[which(names(gal) %in% gal$fixed_params)]
  pos <- which(!names(gal2) %in% gal$fixed_params)
  gal2 <- gal2[pos]
  gal <- utils::modifyList(gal, gal2)
  return(gal)
}

is.gals <- function(x) inherits(x, "gals")

validate <- function(x) UseMethod("validate")

validate.gals <- function(gals){
  if (is.null(gals$duration_count)) gals$duration_count <- FALSE
  if (is.null(gals$offset)) gals$offset <- 0
  if (is.null(gals$preassure_factor)) gals$preassure_factor <- 1
  gals
}

print.gals <- function(x){
  params <- vapply(x, function(x){paste0(rlang::quo_label(x), collapse = ", ")}, character(1))
  vals <- paste0(format(names(x)), " -> ", params, "\n")
  cat(vals, sep="")
}
