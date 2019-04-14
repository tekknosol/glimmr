
analyzer <- function(
  name = "custom",
  time_stamp = "timestamp",
  conc_columns = NULL,
  preassure = NULL,
  preassure_factor = 1,
  temperature = NULL,
  manual_temperature = NULL,
  offset = "0:0",
  duration_count = FALSE,
  spot = "spot",
  day = "day",
  start = "start",
  end = "end",
  trimmer = NULL,
  V = 0.01461,
  A = 0.098
){
  obj <- list(
    name = name,
    time_stamp = time_stamp,
    conc_columns = conc_columns,
    preassure = preassure,
    preassure_factor = preassure_factor,
    temperature = temperature,
    manual_temperature = manual_temperature,
    offset = offset,
    duration_count = duration_count,
    spot = spot,
    day = day,
    start = start,
    end = end,
    trimmer = trimmer,
    V = V,
    A = A
  )
  attr(obj, "class") <- "lyzr"
  obj
}


analyzer_LosGatos <- function(
  manual_temperature = NULL,
  offset = "0:0",
  duration_count = FALSE,
  spot = "spot",
  day = "day",
  start = "start",
  end = "end",
  trimmer = trim_time,
  V = 0.01461,
  A = 0.098
){
  me <- analyzer(
    name = "LosGatos",
    time_stamp = "Time",
    conc_columns = c(CH4 = "[CH4]_ppm", CO2 = "[CO2]_ppm"),
    preassure = "GasP_torr",
    preassure_factor = 1.33322,
    temperature = "AmbT_C",
    manual_temperature = manual_temperature,
    offset = offset,
    duration_count = duration_count,
    spot = spot,
    day = day,
    start = start,
    end = end,
    trimmer = rlang::enquo(trimmer),
    V = V,
    A = A
  )
  # me <- do.call(lyzr,c(settings))
  ## Add the name for the class
  class(me) <- append(class(me),"LosGatos")
  me
}

analyzer_GASMET <- function(
  manual_temperature = "temp",
  offset = "0:0",
  duration_count = FALSE,
  spot = "spot",
  day = "day",
  start = "start",
  end = 10,
  trimmer = NA,
  V = 0.01461,
  A = 0.098
){
  me <- analyzer(
    name = "GASMET",
    time_stamp = "datetime",
    conc_columns = c(CO2 = "CO2", CH4 = "CH4", N2O = "N2O"),
    preassure = "Luftdruck",
    temperature = NA,
    manual_temperature = manual_temperature,
    offset = offset,
    duration_count = duration_count,
    spot = spot,
    day = day,
    start = start,
    end = end,
    trimmer = new_setting(rlang::enquo(trimmer)),
    V = V,
    A = A
  )

  class(me) <- append(class(me),"GASMET")
  me
}

# test <- function()

lyzr_settings <- function(...){
  exprs <- rlang::enquos(...)
  new_set(exprs)
}

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

new_set <- function(x, env = globalenv()) {
  stopifnot(is.list(x))
  x <- lapply(x, new_setting, env = env)
  structure(x, class = "gals")
}

# process_chamber(data, meta, lyzr_settings()) # <- default values + settings
# process_losgatos(data, meta, lyzr_settings()) # <- LosGatos values + settings

process_chamber2 <- function(data = NULL, meta = NULL, ...){
  device <- do.call(analyzer,c(list(...)))
  device
}

process_losgatos2 <- function(data = NULL, meta = NULL, settings){
  device <- LosGatos(settings)
  device
}

process_gasmet2 <- function(data = NULL, meta = NULL, ...){
  device <- do.call(analyzer_GASMET,c(list(...)))
  process_predefined(data, meta, device, FALSE)
}

test <- function(setting){
  device <- do.call(lyzr,c(a))
  device
}


print.lyzr <- function(x, ...){
  params <- vapply(x, function(x){paste0(rlang::quo_label(x), collapse = ", ")}, character(1))
  vals <- paste0(format(names(x)), " -> ", params, "\n")
  cat(vals, sep="")
}
