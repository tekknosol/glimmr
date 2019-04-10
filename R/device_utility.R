#' Read a GASMET file.
#'
#' @param file Path to a file
#'
#' @return A data frame.
#' @export
#'
read_gasmet <- function(file) {
  x <- readr::read_lines(file)
  y <- stringr::str_split(x, "\t")
  switch(y[[1]][2], Datum = lang <- "de", Date = lang <- "en")
  lines <- c()
  for (l in 1:length(x)) {
    if ( (y[[l]][1] == "Messstelle" | y[[l]][1] == "Line") & l > 1) {
      lines <- append(lines, l)
    }
  }
  if (!is.null(lines)) {
    x <- x[-lines]
  }
  x1 <- stringi::stri_trans_general(x, "latin-ascii")
  tmp <- tempfile(fileext = ".csv")
  readr::write_lines(x1, tmp)
  if (lang == "en") {
    y <- readr::read_tsv(tmp, col_types = readr::cols_only(
      Date = "D", Time = "t", CO2 = "n",
      CH4 = "n", N2O = "n", Luftdruck = "n"
    ))
    y$datetime <- lubridate::ymd_hms(paste(y$Date, y$Time))
  } else {
    y <- readr::read_tsv(tmp, col_types = readr::cols_only(
      Datum = "D", Zeit = "t", CO2 = "n",
      CH4 = "n", N2O = "n", Luftdruck = "n"
    ))
    y$datetime <- lubridate::ymd_hms(paste(y$Datum, y$Zeit))
  }

  # y$Datum <- lubridate::ymd(y$Datum) y$Zeit <- lubridate::hms(y$Zeit)
  unlink(tmp)
  return(y)
}

#' Read a LosGatos file.
#'
#' @param path Path to a file or directory.
#' @param format either "dmy" or "mdy".
#' @param clean_dir If TRUE directory is cleaned
#' @param ... additional parameters passed to read_csv()
#'
#' @return A data frame.
#' @export
#'
read_losgatos <- function(path, format = "dmy", clean_dir = FALSE, ...) {
  if (utils::file_test("-f", path)) {
    lg <- read_losgatos_file(path)
  } else {
    lg <- read_losgatos_dir(path, format, clean_dir)
  }

  return(lg)
}

# Helper functions ----------------------------------------------
read_losgatos_file <- function(path, ...) {
  lg <- readr::read_csv(path, skip = 1, ...)
  lg$Time <- lubridate::dmy_hms(lg$Time)
  return(lg)
}

read_losgatos_dir <- function(path, format = "dmy", clean_dir = FALSE) {

  files <- get_losgatos_files(path)
  date <- stringr::str_split(stringr::str_split(files[1], "micro_")[[1]][2],
    "_f")[[1]][1]
  message("read ", length(files), " files")
  suppressWarnings(
    suppressMessages(
      switch(format,
        dmy = {
          complete <- lapply(files, function(x) {
            lg <- readr::read_csv(x, skip = 1) %>%
              dplyr::mutate(Time = lubridate::dmy_hms(.data$Time)) %>%
              dplyr::filter(!is.na(.data$Time)) %>%
              dplyr::filter_if(is.numeric, dplyr::all_vars(!is.na(.data$.)))
          })
        },
        mdy ={
          complete <- lapply(files, function(x) {
            lg <- readr::read_csv(x, skip = 1) %>%
              dplyr::mutate(Time = lubridate::mdy_hms(.data$Time)) %>%
              dplyr::filter(!is.na(.data$Time)) %>%
              dplyr::filter_if(is.numeric, dplyr::all_vars(!is.na(.data$.)))
          })
        }
      )
    )
  )

  if (clean_dir) {
    clean_losgatos_dir(path)
  }

  complete <- dplyr::bind_rows(complete) %>%
    dplyr::arrange(.data$Time) %>%
    dplyr::select(.data$Time, .data$`[CH4]_ppm`, .data$`[CO2]_ppm`, .data$GasP_torr, .data$AmbT_C)
}

clean_losgatos_dir <- function(path) {
  used_files <- get_losgatos_files(path)
  all_files <- dir(path = path, pattern = "*", full.names = T, recursive = F)
  message(length(which(!all_files %in% used_files)), " files deleted.")
  file.remove(all_files[which(!all_files %in% used_files)])
}

get_losgatos_files <- function(path) {
  zips <- dir(path = path, pattern = "f[0-9]*.*zip", full.names = T,
    recursive = F)
  txt <- dir(path = path, pattern = "f[0-9]*.txt", full.names = T,
    recursive = F)
  txt <- txt[which(!txt %in% zips)]
  txt <- txt[which(!txt %in% stringr::str_split(zips, ".zip",
    simplify = T)[, 1])]
  return(append(zips, txt))
}

#' Show plots for every measurement
#'
#' @param fluxdata A data frame with recorded data. Either loaded with
#'   \code{\link{read_gasmet}} or \code{\link{read_losgatos}}.
#' @inheritParams process_chamber
#'
#' @return A series of plots
#' @export
#'
inspect_gasmet <- function(fluxdata, meta) {
  df <- process_gasmet(fluxdata, meta, pre = TRUE)
  df <- df %>% tidyr::gather(key = "gas", value = "concentration", .data$CO2,
    .data$CH4, .data$N2O)
  inspect_fluxdata(df)
  invisible(fluxdata)
}

#' @rdname inspect_gasmet
#' @export
#'
inspect_losgatos <- function(fluxdata, meta) {
  df <- process_losgatos(fluxdata, meta, pre = TRUE)
  df <- df %>% tidyr::gather(key = "gas", value = "concentration", .data$CO2,
    .data$CH4)
  inspect_fluxdata(df)
  invisible(fluxdata)
}

inspect_chamber <- function(fluxdata, meta, analyzer = gals()){

  df <- process_chamber(fluxdata, meta, analyzer = analyzer, pre = TRUE)
  df <- df %>% tidyr::gather(key = "gas", value = "concentration", dplyr::ends_with("ppm"))
  inspect_fluxdata(df)
  invisible(fluxdata)
}

inspect_fluxdata <- function(df) {
  Time <- NULL
  concentration <- NULL
  spot <- NULL
  grDevices::devAskNewPage(ask = TRUE)
  for (i in unique(df$spot)) {
    print(ggplot2::ggplot(df %>% dplyr::filter(spot == i),
      ggplot2::aes(Time * 60, concentration)) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = robust::lmRob, se = FALSE, ggplot2::aes(linetype = "RLM"), color="black", size = .5) +
      ggplot2::geom_smooth(method = lm, se = FALSE, ggplot2::aes(linetype = "LM"), color = "black", size = .5) +
      ggplot2::xlab("minutes") +
      ggplot2::ylab("mixing ratio (ppm)") +
      ggplot2::theme_bw() +
      ggplot2::facet_grid(gas ~ spot + rep, scales = "free")+
      ggplot2::scale_linetype_manual(name="Model type", values=c(1, 4))

    )
  }
  grDevices::devAskNewPage(ask = FALSE)
}

ppm2conc <- function(ppm, temp, pmbar){
  conc <- ppm * 1e-6 * (pmbar * 100) / (8.314 *(temp + 273.15)) * 1000
}

chamber_offset <- function(device, df, meta){
  if (is.numeric(device$offset)){
    df[(device$offset+1):length(rownames(df)),]
  } else {
    df[(meta[[device$offset]]+1):length(rownames(df)),]
  }
}

parse_end <- function(data, device, start, meta){
  if (device$duration_count){
    if (is.numeric(device$end)){
      fl <- which(data[[device$time_stamp]] > start)[1] + device$end
    } else {
        fl <- which(data[[device$time_stamp]] >=start)[1] + (meta[[device$end]] - 1)
      }
    end <- data[fl,][[device$time_stamp]]
  } else {
      if (grepl(":", meta$end)){
        end <- lubridate::ymd_hms(paste(lubridate::date(start), meta$end))
      } else {
          end <- start + end * 60
        }
    }
  return(end)
}


parse_var <- function(conc, meta, x){
  if (x %in% colnames(meta)){
    x <- meta %>% dplyr::pull(!!x)
    return(x)
  } else {
    if (x %in% colnames(conc)){
      x <- mean(conc %>% dplyr::pull(!!x))
      return(x)
    }
  }
  stop("Column ", x, " not found", call. = FALSE)
}

process_flux <- function(hmr_data, meta, device){
  flux <- tibble::tibble(
    date = lubridate::ymd(meta[[device$day]]),
    site = meta[[device$spot]],
    begin = lubridate::ymd_hms(paste(meta[[device$day]], meta[[device$start]]))
  )

  suppressMessages(
    flux <- flux %>%
      dplyr::left_join(
      fit_lm(hmr_data, device) %>%
        dplyr::rename(site = spot, begin = start) %>%
        dplyr::mutate(begin = lubridate::as_datetime(begin))
    ) %>%
      dplyr::left_join(
      fit_rlm(hmr_data, device) %>%
        dplyr::rename(site = spot, begin = start) %>%
        dplyr::mutate(begin = lubridate::as_datetime(begin))
    ) %>%
      dplyr::arrange(date, gas, site)
  )

  # for(col in device$conc_column){
  #   gas <- names(device$conc_columns)[device$conc_columns == col]
  #   colname <- paste(gas, "ppm", sep="")
  #   flux_tmp <- gasfluxes::gasfluxes(hmr_data, .times = "Time", .C = colname,
  #     .id = c("day", gas, "spot", "rep"), methods =  c("robust linear"),
  #     select = NULL
  #   )
  #   flux <- flux %>% tibble::add_column(!!paste(gas, "flux", sep = "_") :=
  #     flux_tmp$robust.linear.f0 * 24, !!paste(gas, "flux", "p", sep = "_") :=
  #     flux_tmp$robust.linear.f0.p)
  # }

  return(flux)
}



chamber_diagnostic <- function(conc, meta, device){
  message("Processing ", device$name, " data.")
  if (device$duration_count & is.numeric(device$end)){
    message("End of interval determined by number of observations. Count = ", device$end)
  } else{
    if (device$duration_count & !is.numeric(device$end)){
      message("End of interval determined by number of observations. Count = column ", device$end)
    }
  }
  if (!is.null(device$manual_temperature)){
    message("Using temperature from meta file. Column = ", device$manual_temperature)
  }

  cols <- which(device$conc_columns %in% colnames(conc) == FALSE)
  if (length(cols) > 0){
    stop("Column(s) ", paste(device$conc_columns[cols], collapse = " "), " not found.", call. = FALSE)
  }
}


fit_lm <- function(hmr_data, device){
  hmr_data %>%
    tidyr::gather(key="gas", val = "conc", paste0(names(device$conc_columns))) %>%
    dplyr::group_by(gas, spot, rep) %>%
    dplyr::group_map(~ broom::tidy(lm(conc~Time, data = .x), quick = T)) %>%
    dplyr::filter(term == "Time") %>%
    dplyr::left_join(
      hmr_data %>%
        tidyr::gather(key="gas", val = "conc", paste0(names(device$conc_columns))) %>%
        dplyr::group_by(spot, rep,gas) %>%
        dplyr::group_map(~ broom::glance(lm(conc~Time, data = .x), quick = T)) %>%
        dplyr::select(LM_r2=r.squared)
    ) %>%
    dplyr::mutate(estimate = estimate * device$V / device$A * 24) %>%
    dplyr::left_join(
      hmr_data %>%
        tidyr::gather(key="gas", val = "conc", paste0(names(device$conc_columns))) %>%
        dplyr::group_by(gas, spot, rep) %>%
        dplyr::summarise(start = first(start)) %>%
        dplyr::ungroup()
    ) %>%
    dplyr::rename(F_LM = estimate) %>%
    dplyr::select(-term)
}

fit_rlm <- function(hmr_data, device){
  hmr_data %>%
    tidyr::gather(key="gas", val = "conc", paste0(names(device$conc_columns))) %>%
    dplyr::group_by(gas, spot, rep) %>%
    dplyr::group_map(~ broom::tidy(robust::lmRob(conc~Time, data = .x), quick = T)) %>%
    dplyr::filter(term == "Time") %>%
    dplyr::left_join(
      hmr_data %>%
        tidyr::gather(key="gas", val = "conc", paste0(names(device$conc_columns))) %>%
        dplyr::group_by(spot, rep,gas) %>%
        dplyr::group_map(~ broom::glance((robust::lmRob(conc~Time, data = .x)))) %>%
        dplyr::select(RLM_r2 = r.squared)
    ) %>%
    dplyr::mutate(estimate = estimate * device$V / device$A * 24) %>%
    dplyr::left_join(
      hmr_data %>%
        tidyr::gather(key="gas", val = "conc", paste0(names(device$conc_columns))) %>%
        dplyr::group_by(gas, spot, rep) %>%
        dplyr::summarise(start = first(start)) %>%
        dplyr::ungroup()
    ) %>%
    dplyr::rename(F_RLM = estimate) %>%
    dplyr::select(-term)
}
