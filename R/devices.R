

preprocess_chamber <- function(conc, meta, device, inspect = FALSE){
  # device <- validate(device)
  chamber_diagnostic(conc, meta, device)

  hmr_data <- dplyr::tibble()

  repcount <- data.frame(plot = unique(meta$plot), count = 0)
  for (i in 1:length(meta$plot)) {
      start <- tryCatch({
          lubridate::ymd_hms(paste(meta[i, ][[device$date]], meta[i, ][[device$start]]))
        }, warning = function(w){
          # print(w)
          if("All formats failed to parse. No formats found." %in% w){
            warning("Meta entry ", i, ": cannot parse start or end timestamp", call. = FALSE, immediate. = TRUE)
            return(NA)
          }
        },error = function(e){
          stop("cannot parse start or end timestamp")
        }

      )
      end <- try(parse_end(conc, device, start, meta[i, ]), silent = T)

    int <- lubridate::interval(start, end)

    if(!is.null(rlang::eval_tidy(device$trimmer))){
      FUN <- match.fun(rlang::eval_tidy(device$trimmer))
      int <- FUN(int)
    }

    repcount[repcount$plot == meta[i, ][[device$plot]], ]$count <- repcount[repcount$plot == meta[i, ][[device$plot]], ]$count + 1

    offset <- meta[i,][[device$offset]] %>% stringr::str_split(":")
    offset <- as.numeric(offset[[1]])
    datapoints <- which(conc[[device$time_stamp]] >= lubridate::int_start(int) & conc[[device$time_stamp]] <= lubridate::int_end(int))

    a <- try(conc[datapoints[(1+offset[1]):(length(datapoints)-offset[2])],], silent = T)

    if (length(datapoints) == 0){
      warning(call. = FALSE, immediate. = T, "meta entry ", i, " skipped.",
              " No matching data.")
      next
    }

    if (is.null(device$manual_temperature)){
      temp_value <- parse_var(a, meta[i, ], device$temperature)
    } else{
      temp_value <- parse_var(a, meta[i, ], device$manual_temperature)
    }
    pressure_value <- parse_var(a, meta[i, ], device$pressure) * device$pressure_factor

    if(is.numeric(device[["V"]])){
      V = device[["V"]]
    } else {
      V = meta[i, ][[device$V]]
    }

    if(is.numeric(device[["A"]])){
      A = device[["A"]]
    } else {
      A = meta[i, ][[device$A]]
    }

    hmr_data_tmp <- tibble::tibble(
      plot = meta[i, ][[device$plot]], date =
      as.character(meta[i, ][[device$date]]), rep =
      repcount[repcount$plot == meta[i, ][[device$plot]], ]$count,
      start = start,
      # start = as.character(lubridate::int_start(int)),
      V = V, A = A,
      Time = as.numeric(a[[device$time_stamp]] - a[1,][[device$time_stamp]]) / 60 / 60
    )

    # calculate concentration in mmol/m³
    for (col in device$conc_columns){
      # colname <- paste("conc_", col, sep="")
      gas <- names(device$conc_columns)[device$conc_columns == col]
      colname <- paste0(gas)
      if (inspect == TRUE){
        value <- a[[col]]
      } else {
        value <- ppm2conc(a %>% dplyr::pull(!!col), temp_value, pressure_value)
      }
      hmr_data_tmp <- hmr_data_tmp %>%
        tibble::add_column(!!colname := value)
    }
    hmr_data <- rbind(hmr_data, hmr_data_tmp)
  }
  # hmr_data <- hmr_data[-1, ]
  if (length(rownames(hmr_data)) == 0){
    stop(call. = F, "No data for flux calculation. Do dates in meta and data file match?")
  }
  hmr_data
}

#' Calculate gasfluxes from dynamic chamber measurement
#'
#' \code{\link{process_losgatos}} and \code{\link{process_gasmet}} are special
#' cases of the general \code{\link{process_chamber}} with preconfigured
#' settings.
#'
#' Data from gas analyzer is split into chunks according to entries in meta
#' files.
#' For each chunk, flux is calculated by fitting linear models and robust
#' linear models (\code{\link[robust]{lmRob}}).
#'
#' @param data Data frame. Recorded data from device.
#' @param meta Data frame. Metadata containing required informations.
#' @param analyzer Set of setup mappings created by \code{\link{analyzer}}. The specified name
#'   value pairs will override default settings.
#' @param ... additional parameters.
#' @param pre If FALSE (the default), flux processing will be executed. If TRUE
#'   flux processing will be skipped and preprocessed data frame will be
#'   returned.
#'
#' @return A tibble
#' \itemize{
#'   \item F_LM: Flux (linear model)
#'   \item LM_r2: R² of linear model fit
#'   \item F_RLM: Flux (robust linear model)
#'   \item RLM_r2: R² of robust linear model fit
#' }
#' @aliases process_losgatos
#' @examples
#' # Process custom analyzer
#' # see `vignette("lyzr")` for more details about the options
#' custom_analyzer <- analyzer(
#'   #define datastructure
#'   time_stamp = "Time",
#'   conc_columns = c(CH4 = "[CH4]_ppm", CO2 = "[CO2]_ppm"),
#'   pressure = "GasP_torr",
#'   pressure_factor = 1.33322,
#'   temperature = "AmbT_C",
#'   trimmer = trim_time,
#'   # define metadata structure
#'   plot = "plot",
#'   date = "date",
#'   start = "start",
#'   end = "end",
#'   #define chamber dimensions
#'   V = 0.01461,
#'   A = 0.098
#' )
#'
#' # Compute fluxes
#' cc_flux <- process_chamber(
#'   # specify data
#'   data = losgatos,
#'   meta = meta_losgatos,
#'   #specify analyzer
#'   analyzer = custom_analyzer
#' )
#'
#' # Process LosGatos
#' lg_flux <- process_losgatos(losgatos, meta_losgatos)
#' # Process LosGatos with custom chamber dimensions
#' lg_flux <- process_losgatos(losgatos, meta_losgatos, V = 2, A = 3)
#'
#' # Process GASMET
#' gm_flux <- process_gasmet(gasmet, meta_gasmet)
#'
#' @export
#'
process_chamber <- function(data = NULL, meta = NULL, analyzer, pre = FALSE){
  # params <- list(...)
  # device <- do.call(analyzer,analyzer)
  process_predefined(data, meta, analyzer, pre)
}


#' @rdname process_chamber
#' @export
process_losgatos <- function(data, meta, ..., pre = FALSE){
  analyzer <- do.call(analyzer_LosGatos,c(list(...)))
  process_predefined(data, meta, analyzer, pre)
}

#' @rdname process_chamber
#' @export
process_gasmet <- function(data, meta, ..., pre = FALSE){
  analyzer <- do.call(analyzer_GASMET,c(list(...)))
  process_predefined(data, meta, analyzer, pre)
}

process_predefined <- function(data, meta, device, pre = FALSE){
  # if(is.gals(analyzer)){
  #   device <- stats::update(device, analyzer)
  # }

  if (pre == TRUE) {
    hmr_data <- preprocess_chamber(data, meta, device, inspect = TRUE)
    return(hmr_data)
  }

  hmr_data <- preprocess_chamber(data, meta, device)

  flux <- process_flux(hmr_data, meta, device)
  return(flux)
}
