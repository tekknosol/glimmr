
#' Read a LosGatos file.
#'
#' @param file Path to a file.
#' @param ... additional parameters passed to read_csv()
#'
#' @return A data frame.
#' @export
#'
read_losgatos <- function(file, ...){
  lg <- readr::read_csv(file, skip=1, ...)
  lg$Time <- lubridate::dmy_hms(lg$Time)
  return(lg)
}

#' Read a GASMET file.
#'
#' @param file Path to a file
#'
#' @return A data frame.
#' @export
#'
read_gasmet <- function(file){
  x <- readr::read_lines(file)
  y <- stringr::str_split(x, "\t")
  switch (y[[1]][2],
    Datum = lang <- "de",
    Date = lang <- "en"
  )
  lines <-c()
  for(l in 1:length(x)){
    if((y[[l]][1]=="Messstelle"|y[[l]][1]=="Line")&l>1){
      lines <- append(lines, l)
    }
  }
  if(!is.null(lines)){
    x <- x[-lines]
  }
  x1 <- stringi::stri_trans_general(x, "latin-ascii")
  tmp <- tempfile(fileext = ".csv")
  readr::write_lines(x1, tmp)
  if(lang=="en"){
    y <- readr::read_tsv(tmp,
                         col_types = readr::cols_only(
                           Date = "D",
                           Time = "t",
                           CO2 = "n",
                           CH4 = "n",
                           N2O = "n",
                           Luftdruck = "n"
                         )
    )
    y$datetime <- lubridate::ymd_hms(paste(y$Date, y$Time))
  } else {
    y <- readr::read_tsv(tmp,
                         col_types = readr::cols_only(
                           Datum = "D",
                           Zeit = "t",
                           CO2 = "n",
                           CH4 = "n",
                           N2O = "n",
                           Luftdruck = "n"
                         )
    )
    y$datetime <- lubridate::ymd_hms(paste(y$Datum, y$Zeit))
  }

  # y$Datum <- lubridate::ymd(y$Datum)
  # y$Zeit <- lubridate::hms(y$Zeit)
  unlink(tmp)
  return(y)
}
