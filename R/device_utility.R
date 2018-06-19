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
        if ((y[[l]][1] == "Messstelle" | y[[l]][1] == "Line") & l > 1) {
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
        y <- readr::read_tsv(tmp, col_types = readr::cols_only(Date = "D", Time = "t", CO2 = "n",
            CH4 = "n", N2O = "n", Luftdruck = "n"))
        y$datetime <- lubridate::ymd_hms(paste(y$Date, y$Time))
    } else {
        y <- readr::read_tsv(tmp, col_types = readr::cols_only(Datum = "D", Zeit = "t", CO2 = "n",
            CH4 = "n", N2O = "n", Luftdruck = "n"))
        y$datetime <- lubridate::ymd_hms(paste(y$Datum, y$Zeit))
    }

    # y$Datum <- lubridate::ymd(y$Datum) y$Zeit <- lubridate::hms(y$Zeit)
    unlink(tmp)
    return(y)
}

#' Read a LosGatos file.
#'
#' @param file Path to a file or directory.
#' @param clean_dir If TRUE directory is cleaned
#' @param ... additional parameters passed to read_csv()
#'
#' @return A data frame.
#' @export
#'
read_losgatos <- function(path, clean_dir = FALSE, ...) {
    if (file_test("-f", path)) {
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
    files <- get_losgatos_files(path)
    date <- stringr::str_split(stringr::str_split(files[1], "micro_")[[1]][2], "_f")[[1]][1]
    complete <- lapply(files, function(x) {
        lg <- readr::read_csv(x, skip = 1) %>%
          mutate(Time = lubridate::dmy_hms(Time)) %>%
          filter(lubridate::date(Time) == date) %>%
          # mutate(Time = as.character(Time)) %>%
          filter(!is.na(Time))
    })

    complete <- bind_rows(complete) %>% arrange(Time)

    if (clean_dir) {
        clean_losgatos_dir(path)
    }

    return(complete)
}

clean_losgatos_dir <- function(path) {

    used_files <- get_losgatos_files(path)
    all_files <- dir(path = path, pattern = "*", full.names = T, recursive = F)
    message(length(which(!all_files %in% used_files)), " files deleted.")
    file.remove(f2[which(!all_files %in% used_files)])

}

get_losgatos_files <- function(path) {
    zips = dir(path = path, pattern = "f[0-9]*.*zip", full.names = T, recursive = F)
    txt <- dir(path = path, pattern = "f[0-9]*.txt", full.names = T, recursive = F)
    txt <- txt[which(!txt %in% zips)]
    txt <- txt[which(!txt %in% stringr::str_split(zips, ".zip", simplify = T)[, 1])]
    return(append(zips, txt))
}
