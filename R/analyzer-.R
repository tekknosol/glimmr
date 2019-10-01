#' Gas analyzer setup
#'
#' Analyzer mappings describe how columns of meta file are mapped to properties
#' of the gas analyzer
#'
#' @param time_stamp Timestamp
#' @param conc_columns A named vector declaring the columns containing the
#'   actual mixing rations. E.g. `c(CO2  = "CO2")`.
#' @param preassure Column containing the atmospheric preasure in mbar.
#' @param preassure_factor If preasure is provided in a different unit than
#'   mbar, a conversion factor can be specified.
#' @param temperature Column containing the temperature in °C.
#' @param manual_temperature Column conatining an additional measured temperature.
#'   If specified, the `temperature` value is ommited.
#' @param offset (Default = `0:0`). Colon separated value for offsetting the
#'   datasets. First value offsets the first, second value offsets the last
#'   datapoint. Positive values shrinken the dataset i.e `2:3` skips two
#'   datapoints at the beginning and truncates three datapoints at the end of
#'   the dataset. Negative values expand the dataset i.e. `-2:-3` add two
#'   datapoints at the beginning and three datapoints at the end of the dataset.
#' @param duration_count If TRUE, `end` is determined by the number of datapoints
#'   within each measurement. If FALSE, `end` is treated as duration or point in time.
#' @param plot (Default = "plot"). Column containing the ID of the measurement location.
#' @param date (Default = "date"). Column containing the day of measurement. Format: "YYYY-MM-DD".
#' @param start (Default = "start"). Column containing the start of measurement. Format: "HH:MM" or "HH:MM:SS".
#' @param end (Default = "end"). Column containing the end of measurement. There are several options:
#'
#'   Point in Time. Format: "HH:MM" or "HH:MM:SS" (e.g. `"13:43"`).
#'
#'   Duration in minutes (e.g. `5`).
#'
#'   Number of Datapoints regardless of the effectively elapsed time (e.g. `10`).
#'   This behaviour is controlled by `duration_count`.
#'
#'   Alternatively a single number treated as Duration or Number of Datapoints for all measurements (e.g. `5`, behaviour is again controlled by `duration_count`).
#' @param trimmer A function to trim the intervall between `start` and `end`. See `trim_time()` for details.
#' @param V Volume of the chamber.
#' @param A Area of the chamber.
#'
#' @return An object of clas lyzr.
#' @export
analyzer <- function(
  time_stamp = "timestamp",
  conc_columns = NULL,
  preassure = NULL,
  preassure_factor = 1,
  temperature = NULL,
  manual_temperature = NULL,
  offset = "offset",
  duration_count = FALSE,
  plot = "plot",
  date = "date",
  start = "start",
  end = "end",
  trimmer = NULL,
  V = 0.01461,
  A = 0.098
){
  obj <- list(
    time_stamp = time_stamp,
    conc_columns = conc_columns,
    preassure = preassure,
    preassure_factor = preassure_factor,
    temperature = temperature,
    manual_temperature = manual_temperature,
    offset = offset,
    duration_count = duration_count,
    plot = plot,
    date = date,
    start = start,
    end = end,
    trimmer = new_setting(rlang::enquo(trimmer)),
    V = V,
    A = A
  )
  attr(obj, "class") <- "lyzr"
  obj
}

# lyzr_settings <- function(...){
#   exprs <- rlang::enquos(...)
#   new_set(exprs)
# }
#
new_setting <- function(x, env = globalenv()) {
  if (rlang::is_quosure(x)) {
    if (!rlang::quo_is_symbolic(x)) {
      x <- rlang::quo_get_expr(x)
    }
    return(x)
  }

  if (rlang::is_symbolic(x)) {
    x <- rlang::new_quosure(x, env = env)
    return(x)
  }

  x
}
#
# new_set <- function(x, env = globalenv()) {
#   stopifnot(is.list(x))
#   x <- lapply(x, new_setting, env = env)
#   structure(x, class = "gals")
# }

is.lyzr <- function(x) inherits(x, "lyzr")

validate <- function(obj) UseMethod("validate")

#' @export
print.lyzr <- function(x, ...){
  params <- vapply(x, function(x){paste0(rlang::quo_label(x), collapse = ", ")}, character(1))
  vals <- paste0(format(names(x)), " -> ", params, "\n")
  cat(vals, sep="")
}

#' @export
validate.lyzr <- function(lyzr){
  if (is.null(lyzr$duration_count)) lyzr$duration_count <- FALSE
  if (is.null(lyzr$offset)) lyzr$offset <- "0:0"
  if (is.null(lyzr$preassure_factor)) lyzr$preassure_factor <- 1
  lyzr
}
