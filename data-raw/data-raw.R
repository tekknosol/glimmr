library(dplyr)
library(lubridate)
gasmet <- read_gasmet("data-raw/gasmet.csv")
meta_gasmet <- readr::read_csv("data-raw/meta.csv")

meta_gasmet <- meta_gasmet %>%
  mutate(begin=dmy_hms(paste(day,begin))) %>%
  select(spot, day, begin, temp, wndw, offset)

meta_gasmet <- meta_gasmet[1:3,]

gasmet <- gasmet %>% filter(datetime>=meta_gasmet$begin[1]& datetime <= meta_gasmet$begin[3])

devtools::use_data(gasmet, meta_gasmet, overwrite = T)
