

preprocess_chamber <- function(conc, meta, device){
  device <- validate(device)
  chamber_diagnostic(conc, meta, device)
  hmr_data <- dplyr::tibble(
    spot = NA, day = NA, rep = NA, start = NA, V = NA, A = NA, Time = NA
  )

  for (col in device$conc_columns){
    gas <- names(device$conc_columns)[device$conc_columns == col]
    colname <- paste(gas, "mmol", sep="")
    hmr_data <- hmr_data %>%
      tibble::add_column(!!colname := NA, !!gas := NA)
  }

  repcount <- data.frame(spot = unique(meta$spot), count = 0)
  for (i in 1:length(meta$spot)) {
    suppressWarnings(
      start <- lubridate::ymd_hms(paste(meta[i, ] %>% dplyr::pull(device$day), meta[i, ]%>% dplyr::pull(device$start)))
    )
      end <- parse_end(conc, device, start, meta[i, ])

    int <- lubridate::interval(start, end)

    if(length(device$trimmer) != 0){
      FUN <- match.fun(device$trimmer)
      int <- FUN(int)
    }


    repcount[repcount$spot == meta[i, ] %>% dplyr::pull(device$spot), ]$count <- repcount[repcount$spot == meta[i, ] %>% dplyr::pull(device$spot), ]$count + 1
    a <- conc %>%
      dplyr::filter(!!rlang::sym(device$time_stamp) >= lubridate::int_start(int) & !!rlang::sym(device$time_stamp) <= lubridate::int_end(int))
    a <- chamber_offset(df = a, device = device, meta = meta)
    if (length(rownames(a)) == 0){
      warning(call. = FALSE, "meta entry ", i, " skipped.",
              " No matching data.")
      next
    }

    if (is.null(device$manual_temperature)){
      temp_value <- parse_var(a, meta[i, ], device$temperature)
    } else{
      temp_value <- parse_var(a, meta[i, ], device$manual_temperature)
    }
    preassure_value <- parse_var(a, meta[i, ], device$preassure) * device$preassure_factor

    hmr_data_tmp <- tibble::tibble(
      spot = meta[i, ][[device$spot]], day =
      as.character(meta[i, ][[device$day]]), rep =
      repcount[repcount$spot == meta[i, ][[device$spot]], ]$count,
      start = as.character(lubridate::int_start(int)), V = device[["V"]], A = device[["A"]],
      Time = as.numeric(a[[device$time_stamp]] - a[1,][[device$time_stamp]]) / 60 / 60
    )

    # calculate concentration in mmol/m³
    for (col in device$conc_columns){
      # colname <- paste("conc_", col, sep="")
      gas <- names(device$conc_columns)[device$conc_columns == col]
      colname <- paste(gas, "mmol", sep="")
      hmr_data_tmp <- hmr_data_tmp %>%
        tibble::add_column(!!colname := ppm2conc(a %>% dplyr::pull(!!col), temp_value, preassure_value), !!gas := gas)
    }
    hmr_data <- rbind(hmr_data, hmr_data_tmp)
  }
  hmr_data <- hmr_data[-1, ]
}

#' Calculate gasfluxes from dynamic chamber measurement
#'
#' `process_losgatos()`and `process_gasmet()` are special cases of the general
#' `process_chamber()` with preconfigured settings. See [gals_losgatos()] and
#' [gals_gasmet()] for details.
#'
#' @param data Data frame. Recorded data from device.
#' @param meta Data frame. Metadata containing required informations.
#' @param analyzer Set of setup mappings created by [gals()]. The specified name
#'   value pairs will override default settings.
#' @param pre If FALSE (the default), flux processing will be executed. If TRUE
#'   flux processing will be skipped and preprocessed data frame will be
#'   returned.
#'
#' @return A data frame.
#' @aliases process_losgatos
#' @export
#'
process_chamber <- function(data = NULL, meta = NULL, analyzer = gals(), pre = T){

  hmr_data <- preprocess_chamber(data, meta, analyzer)

  if (pre == TRUE) {
    return(hmr_data)
  }

  flux <- process_flux(hmr_data, meta, analyzer)
  return(flux)
}

#' @rdname process_chamber
#' @export
process_losgatos <- function(data, meta, analyzer = NULL, pre = FALSE){
  device <- gals_losgatos()

  if(is.gals(analyzer)){
    device <- stats::update(device, analyzer)
  }

  hmr_data <- preprocess_chamber(data, meta, device)

  if (pre == TRUE) {
    return(hmr_data)
  }

  flux <- process_flux(hmr_data, meta, device)
  return(flux)
}

#' @rdname process_chamber
#' @export
process_gasmet <- function(data, meta, analyzer = NULL, pre = FALSE){
  device <- gals_gasmet()
  if(is.gals(analyzer)){
    device <- stats::update(device, analyzer)
  }
  hmr_data <- preprocess_chamber(data, meta, device)

  if (pre == TRUE) {
    return(hmr_data)
  }

  flux <- process_flux(hmr_data, meta, device)
  return(flux)
}
