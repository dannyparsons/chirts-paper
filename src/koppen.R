library(here)
library(dplyr)

source(here("src", "helper_funs.R"))

# Useful link http://www.meteotemplate.com/template/plugins/climateClassification/koppen.php
# Data from http://hanschen.org/koppen
koppen <- read.table(here("data", "koppen", "koppen_1901-2010.tsv"), header = TRUE)

stations <- c("Sadore", "Wa", "Tamale", "Saltpond", "Kisumu", "Dodoma", "Mpika", "Livingstone")
station_metadata <- readRDS(here("data", "stations_metadata.RDS"))
temp_metadata <- station_metadata %>%
  filter(station %in% stations)

grd_pts <- closest_point(points = koppen %>% dplyr::select(longitude, latitude), 
              target = temp_metadata %>% dplyr::select(longitude, latitude))

grd_pts$station <- temp_metadata$station

grd_pts <- left_join(grd_pts, koppen, by = c("longitude", "latitude"))
