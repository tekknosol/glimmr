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
#' @param clean_dir If TRUE directory is cleaned
#' @param ... additional parameters passed to read_csv()
#'
#' @return A data frame.
#' @export
#'
read_losgatos <- function(path, clean_dir = FALSE, ...) {
  if (utils::file_test("-f", path)) {
    lg <- read_losgatos_file(path)
  } else {
    lg <- read_losgatos_dir(path, clean_dir)
  }

  return(lg)
}

# Helper functions ----------------------------------------------
read_losgatos_file <- function(path, ...) {
  lg <- readr::read_csv(path, skip = 1, ...)
  lg$Time <- lubridate::dmy_hms(lg$Time)
  return(lg)
}

read_losgatos_dir <- function(path, clean_dir = FALSE) {

  Time <- NULL
  `[CH4]_ppm` <- NULL
  `[CO2]_ppm` <- NULL
  GasP_torr <- NULL
  AmbT_C <- NULL
  . <- NULL
  files <- get_losgatos_files(path)
  date <- stringr::str_split(stringr::str_split(files[1], "micro_")[[1]][2],
    "_f")[[1]][1]
  message("read ", length(files), " files")
  suppressWarnings(
    suppressMessages(
      complete <- lapply(files, function(x) {
        lg <- readr::read_csv(x, skip = 1) %>%
          dplyr::mutate(Time = lubridate::dmy_hms(Time)) %>%
          dplyr::filter(!is.na(Time)) %>%
          dplyr::filter_if(is.numeric, dplyr::all_vars(!is.na(.)))
      })
    )
  )

  if (clean_dir) {
    clean_losgatos_dir(path)
  }

  complete <- dplyr::bind_rows(complete) %>%
    dplyr::arrange(Time) %>%
    dplyr::select(Time, `[CH4]_ppm`, `[CO2]_ppm`, GasP_torr, AmbT_C)
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
#' @param meta Metadata
#' @aliases inspect_losgatos
#'
#' @return A series of plots
#' @export
#'
inspect_gasmet <- function(fluxdata, meta) {
  CO2mmol <- NULL
  CH4mmol <- NULL
  N2Ommol <- NULL
  df <- process_gasmet(fluxdata, meta, pre = TRUE)
  df <- df %>% tidyr::gather(key = "gas", value = "concentration", CO2mmol,
    CH4mmol, N2Ommol)
  inspect_fluxdata(df)
  invisible(fluxdata)
}

#' @rdname inspect_gasmet
#' @export
#'
inspect_losgatos <- function(fluxdata, meta) {
  CO2mmol <-NULL
  CH4mmol <- NULL
  df <- process_losgatos(fluxdata, meta, pre = TRUE)
  df <- df %>% tidyr::gather(key = "gas", value = "concentration", CO2mmol,
    CH4mmol)
  inspect_fluxdata(df)
  invisible(fluxdata)
}

inspect_fluxdata <- function(df) {
  Time <- NULL
  concentration <- NULL
  spot <- NULL
  devAskNewPage(ask = TRUE)
  for (i in unique(df$spot)) {
    print(ggplot2::ggplot(df %>% dplyr::filter(spot == i),
      ggplot2::aes(Time * 60, concentration)) +
      ggplot2::geom_point() +
      ggplot2::xlab("minutes") +
      ggplot2::theme_bw() +
      ggplot2::facet_grid(gas ~ spot + rep, scales = "free")
    )
  }
  devAskNewPage(ask = FALSE)
}
