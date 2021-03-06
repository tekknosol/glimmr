library(dplyr)
library(lubridate)
library(glimmr)
gasmet <- read_gasmet("data-raw/gasmet.csv")
meta_gasmet <- readr::read_csv("data-raw/meta_gasmet.csv")

meta_gasmet <- meta_gasmet %>%
  # mutate(begin = dmy_hms(paste(day, begin))) %>%
  select(plot, date, start, temp, offset)

meta_gasmet <- meta_gasmet[1:6, ]
meta_gasmet$date <- ymd("2017-04-12")
meta_gasmet$start <- as.character(meta_gasmet$start)
meta_gasmet$plot <- rep(c("plotA", "plotB"), each = 3)

meta_losgatos <- readr::read_csv("data-raw/meta_losgatos.csv")
meta_losgatos$start <- as.character(meta_losgatos$start)
meta_losgatos$end <- as.character(meta_losgatos$end)
meta_losgatos$plot <- "plotA"
# meta_losgatos <- meta_losgatos %>%
  # mutate(start = ymd_hms(paste(day, start)), end = ymd_hms(paste(day, end)))
losgatos <- read_losgatos("data-raw/2018-05-23/")

usethis::use_data(gasmet, meta_gasmet, losgatos, meta_losgatos, overwrite = T)
