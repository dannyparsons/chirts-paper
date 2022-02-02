library(here)
library(dplyr)
library(ggplot2)
library(lubridate)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(tidync)
library(ncdf4)
library(sp)

source(here("src", "helper_funs.R"))

daily_temp <- readRDS(here("data", "temperature_1979_qc.RDS"))

station_metadata <- readRDS(here("data", "stations_metadata.RDS"))
temp_metadata <- station_metadata %>%
  filter(station %in% unique(daily_temp$station))

# station location check --------------------------------------------------
sf_af <- ne_countries(returnclass = "sf", continent = "africa")
ggplot(sf_af) + 
  geom_sf() +
  geom_point(data = temp_metadata, aes(x = longitude, y = latitude)) +
  geom_text_repel(data = temp_metadata, aes(x = longitude, y = latitude, label = station))

rm(sf_af)

# ERA5 Import -------------------------------------------------------------

# files <- list.files("D:/data/era5", pattern = "africa-2m_temperature", full.names = TRUE)
files <- list.files(here("data", "era5"), pattern = "africa-2m_temperature", full.names = TRUE)

nc <- nc_open(files[1])
stopifnot(min(nc$dim$longitude$vals) < min(temp_metadata$longitude))
stopifnot(max(nc$dim$longitude$vals) > max(temp_metadata$longitude))
stopifnot(min(nc$dim$latitude$vals) < min(temp_metadata$latitude))
stopifnot(max(nc$dim$latitude$vals) > max(temp_metadata$latitude))
xs <- nc$dim$longitude$vals
ys <- nc$dim$latitude$vals
resx <- xs[2] - xs[1]
resy <- ys[2] - ys[1]
max_dist <- sqrt((resx/2)^2 + (resy/2)^2)
xy_points <- expand.grid(xs, ys)
xy_extract <- closest_point(points = xy_points, 
                            target = temp_metadata %>% dplyr::select(longitude, latitude))
xy_extract$station <- temp_metadata$station
xy_extract$req_longitude <- temp_metadata$longitude
xy_extract$req_latitude <- temp_metadata$latitude
xy_extract$dist <- apply(xy_extract, 1, function(r) {
  sp::spDistsN1(matrix(as.numeric(c(r[["Var1"]], r[["Var2"]])), ncol = 2),
                as.numeric(c(r[["req_longitude"]], r[["req_latitude"]])))
}
)
stopifnot(all(xy_extract$dist <= max_dist))
print(nc$dim$time$units)
nc_close(nc)

saveRDS(xy_extract, here("data", "temp_locations_era5.RDS"))

era5_dfs <- list()
pb <- txtProgressBar(min = 0, max = length(files) * nrow(xy_extract), style = 3)
count <- 0
for(f in files) {
  for(i in seq_len(nrow(xy_extract))) {
    df <- tidync(f) %>%
      hyper_filter(longitude = longitude == xy_extract[i, 1], 
                   latitude = latitude == xy_extract[i, 2]) %>%
      hyper_tibble()
    df$station <- xy_extract$station[i]
    df$req_longitude <- xy_extract$req_longitude[i]
    df$req_latitude <- xy_extract$req_latitude[i]
    df$dist <- xy_extract$dist[i]
    era5_dfs[[length(era5_dfs) + 1]] <- df
    count <- count + 1
    setTxtProgressBar(pb, count)
  }
}
temp_era5_hour <- bind_rows(era5_dfs) %>%
  group_by(station) %>%
  mutate(date_time = as.POSIXct(time * 60 * 60,
                                origin = as.POSIXct("1900/01/01 00:00:00"), tz = "GMT"),
         date = as.Date(date_time),
         # Temperature recording time ~ 6UTC
         # ERA5 record at 6am is instantaneous
         # So take 7UTC as the first reading
         # Start later in West Africa to avoid missing the early morning minimum 
         # (minimum thermometers often read at ~6am but reset ~9am)
         # Formula: lead = start_time - UTC_diff
         # e.g. start_time = 7am, UTC_diff = 0, lead = 7 - 0 = 7
         lead = ifelse(station %in% c("Wa", "Tamale", "Saltpond", "Sadore"), 10, 7),
         t2m_lead = dplyr::lead(t2m - 273.15, first(lead)),
         hour = dplyr::lead(as.numeric(format(date_time, format = "%H")), first(lead))
  )
temp_era5 <- temp_era5_hour %>%
  group_by(station, date) %>%
  summarise(tmin_era5 = min(t2m_lead), 
            tmax_era5 = max(t2m_lead)) %>%
  mutate(tmin_era5 = dplyr::lead(tmin_era5))

temp_era5_tmintime <- temp_era5_hour %>%
  group_by(station, date) %>%
  filter(t2m_lead == min(t2m_lead)) %>%
  mutate(n = n()) %>%
  filter(n == 1) %>%
  summarise(tmin_hour = dplyr::first(hour))

temp_era5_tmaxtime <- temp_era5_hour %>%
  group_by(station, date) %>%
  filter(t2m_lead == max(t2m_lead)) %>%
  mutate(n = n()) %>%
  filter(n == 1) %>%
  summarise(tmax_hour = dplyr::first(hour))

temp_era5_time <- full_join(temp_era5_tmintime, temp_era5_tmaxtime, by = c("station", "date"))

ggplot(temp_era5_time, aes(x = tmin_hour)) +
  geom_bar() +
  facet_wrap(~station)

temp_era5 <- left_join(temp_era5, temp_era5_time, by = c("station", "date"))

temp_gridded <- left_join(daily_temp, temp_era5, by = c("station", "date"))


# ERA5 Land Import --------------------------------------------------------

files <- list.files("D:/data/era5_land", pattern = "africa-2m_temperature", full.names = TRUE)
files <- list.files(here("data", "era5_land"), pattern = "africa-2m_temperature", full.names = TRUE)

nc <- nc_open(files[1])
stopifnot(min(nc$dim$longitude$vals) < min(temp_metadata$longitude))
stopifnot(max(nc$dim$longitude$vals) > max(temp_metadata$longitude))
stopifnot(min(nc$dim$latitude$vals) < min(temp_metadata$latitude))
stopifnot(max(nc$dim$latitude$vals) > max(temp_metadata$latitude))
xs <- nc$dim$longitude$vals
ys <- nc$dim$latitude$vals
resx <- xs[2] - xs[1]
resy <- ys[2] - ys[1]
max_dist <- sqrt((resx/2)^2 + (resy/2)^2)
xy_points <- expand.grid(xs, ys)
xy_extract <- closest_point(points = xy_points, 
                            target = temp_metadata %>% dplyr::select(longitude, latitude))
xy_extract$station <- temp_metadata$station
xy_extract$req_longitude <- temp_metadata$longitude
xy_extract$req_latitude <- temp_metadata$latitude
xy_extract$dist <- apply(xy_extract, 1, function(r) {
  sp::spDistsN1(matrix(as.numeric(c(r[["Var1"]], r[["Var2"]])), ncol = 2),
                as.numeric(c(r[["req_longitude"]], r[["req_latitude"]])))
}
)
stopifnot(all(xy_extract$dist <= max_dist))
print(nc$dim$time$units)
nc_close(nc)

saveRDS(xy_extract, here("data", "temp_locations_era5land.RDS"))

era5_land_dfs <- list()
pb <- txtProgressBar(min = 0, max = length(files) * nrow(xy_extract), style = 3)
count <- 0
for(f in files) {
  for(i in seq_len(nrow(xy_extract))) {
    df <- tidync(f) %>%
      hyper_filter(longitude = longitude == xy_extract[i, 1], 
                   latitude = latitude == xy_extract[i, 2]) %>%
      hyper_tibble()
    df$station <- xy_extract$station[i]
    df$req_longitude <- xy_extract$req_longitude[i]
    df$req_latitude <- xy_extract$req_latitude[i]
    df$dist <- xy_extract$dist[i]
    era5_land_dfs[[length(era5_land_dfs) + 1]] <- df
    count <- count + 1
    setTxtProgressBar(pb, count)
  }
}
temp_era5land_hour <- bind_rows(era5_land_dfs) %>%
  group_by(station) %>%
  mutate(date_time = as.POSIXct(time * 60 * 60,
                                origin = as.POSIXct("1900/01/01 00:00:00"), tz = "GMT"),
         date = as.Date(date_time),
         # Temperature recording time ~ 6UTC
         # ERA5 record at 6am is instantaneous
         # So take 7UTC as the first reading
         # Start later in West Africa to avoid missing the early morning minimum 
         # (minimum thermometers often read at ~6am but reset ~9am)
         # Formula: lead = start_time - UTC_diff
         # e.g. start_time = 7am, UTC_diff = 0, lead = 7 - 0 = 7
         lead = ifelse(station %in% c("Wa", "Tamale", "Saltpond", "Sadore"), 10, 7),
         t2m_lead = dplyr::lead(t2m - 273.15, first(lead)),
         hour = dplyr::lead(as.numeric(format(date_time, format = "%H")), first(lead))
  )
saveRDS(temp_era5land_hour, here("data", "era5_land", "era5_land_t2m.RDS"))

temp_era5land <- temp_era5land_hour %>%
  group_by(station, date) %>%
  summarise(tmin_era5land = min(t2m_lead), 
            tmax_era5land = max(t2m_lead)) %>%
  mutate(tmin_era5land = dplyr::lead(tmin_era5land))

saveRDS(temp_era5land, here("data", "era5_land", "era5_land_t2m_daily.RDS"))

temp_era5land_tmintime <- temp_era5land_hour %>%
  group_by(station, date) %>%
  filter(t2m_lead == min(t2m_lead)) %>%
  mutate(n = n()) %>%
  filter(n == 1) %>%
  summarise(tmin_hour = dplyr::first(hour))

temp_era5land_tmaxtime <- temp_era5land_hour %>%
  group_by(station, date) %>%
  filter(t2m_lead == max(t2m_lead)) %>%
  mutate(n = n()) %>%
  filter(n == 1) %>%
  summarise(tmax_hour = dplyr::first(hour))

temp_era5land_time <- full_join(temp_era5land_tmintime, temp_era5land_tmaxtime, by = c("station", "date"))

ggplot(temp_era5land_time, aes(x = tmin_hour)) +
  geom_bar() +
  facet_wrap(~station)

temp_era5land <- left_join(temp_era5land, temp_era5land_time, by = c("station", "date"))

temp_gridded <- left_join(daily_temp, temp_era5land, by = c("station", "date"))

# Merge all ---------------------------------------------------------------

saveRDS(temp_gridded, here("data", "temp_gridded.RDS"))
