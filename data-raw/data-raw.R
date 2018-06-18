library(dplyr)
library(lubridate)
library(glimmr)
gasmet <- read_gasmet("data-raw/gasmet.csv")
meta_gasmet <- readr::read_csv("data-raw/meta.csv")

meta_gasmet <- meta_gasmet %>%
  mutate(begin=dmy_hms(paste(day,begin))) %>%
  select(spot, day, begin, temp, wndw, offset)

meta_gasmet <- meta_gasmet[1:6,]

mask <- which((gasmet$datetime>=meta_gasmet$begin[1]&gasmet$datetime <= meta_gasmet$begin[3])|(gasmet$datetime>=meta_gasmet$begin[4]&gasmet$datetime <= meta_gasmet$begin[6]))

mask <- append(mask,(mask[length(mask)]+1):(mask[length(mask)]+15))

gasmet <- gasmet[mask,]

devtools::use_data(gasmet, meta_gasmet, overwrite = T)
