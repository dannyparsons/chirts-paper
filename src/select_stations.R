library(here)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(purrr)

source(here("src", "helper_funs.R"))

daily <- readRDS(here("data", "stations_all.RDS"))
stations <- c("Dodoma", "Saltpond", "Tamale", "Wa", "Livingstone", "Mpika",
              "Kisumu", "Sadore")
daily <- daily %>% 
  filter(station %in% stations & year(date) >= 1979) %>%
  select(country, station, date, tmax, tmin)

saveRDS(daily, here("data", "stations_temperature_1979.RDS"))
write.csv(daily, here("data", "stations_temperature_1979.csv"), row.names = FALSE)
