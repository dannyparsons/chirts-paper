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
sf_af <- ne_countries(returnclass = "sf", continent = "Africa")
ggplot(sf_af) + 
  geom_sf() +
  geom_point(data = temp_metadata, aes(x = longitude, y = latitude)) +
  geom_text_repel(data = temp_metadata, aes(x = longitude, y = latitude, label = station))

rm(sf_af)


# CHIRTS tmin Import Zambia -----------------------------------------------

zambia_metadata <- temp_metadata %>% filter(country == "Zambia")
# tmin_files <- list.files("D:/data/chirts", pattern = "zambia_tmin", full.names = TRUE)
tmin_files <- list.files(here("data", "chirts"), pattern = "zambia_tmin", full.names = TRUE)

nc <- nc_open(tmin_files[1])
stopifnot(min(nc$dim$X$vals) < min(zambia_metadata$longitude))
stopifnot(max(nc$dim$X$vals) > max(zambia_metadata$longitude))
stopifnot(min(nc$dim$Y$vals) < min(zambia_metadata$latitude))
stopifnot(max(nc$dim$Y$vals) > max(zambia_metadata$latitude))
xs <- nc$dim$X$vals
ys <- nc$dim$Y$vals
resx <- xs[2] - xs[1]
resy <- ys[2] - ys[1]
max_dist <- sqrt((resx/2)^2 + (resy/2)^2)
xy_points <- expand.grid(xs, ys)
xy_extract <- closest_point(points = xy_points, 
                            target = zambia_metadata %>% dplyr::select(longitude, latitude))
xy_extract$station <- zambia_metadata$station
xy_extract$req_longitude <- zambia_metadata$longitude
xy_extract$req_latitude <- zambia_metadata$latitude
xy_extract$dist <- apply(xy_extract, 1, function(r) {
  sp::spDistsN1(matrix(as.numeric(c(r[["Var1"]], r[["Var2"]])), ncol = 2),
                as.numeric(c(r[["req_longitude"]], r[["req_latitude"]])))
}
)
stopifnot(all(xy_extract$dist <= max_dist))
print(nc$dim$`T`$units)
nc_close(nc)

locs_chirts <- xy_extract

tmin_dfs <- list()
pb <- txtProgressBar(min = 0, max = length(tmin_files) * nrow(xy_extract), style = 3)
count <- 0
for(f in tmin_files) {
  for(i in seq_len(nrow(xy_extract))) {
    df <- tidync(f) %>%
      hyper_filter(X = X == xy_extract[i, 1], 
                   Y = Y == xy_extract[i, 2]) %>%
      hyper_tibble()
    df$station <- xy_extract$station[i]
    df$req_longitude <- xy_extract$req_longitude[i]
    df$req_latitude <- xy_extract$req_latitude[i]
    df$dist <- xy_extract$dist[i]
    tmin_dfs[[length(tmin_dfs) + 1]] <- df
    count <- count + 1
    setTxtProgressBar(pb, count)
  }
}
chirts_tmin <- bind_rows(tmin_dfs) %>%
  group_by(station) %>%
  mutate(date = as.Date(T, origin = structure(-2440588, class = "Date"))) %>%
  select(station, date, tmin_chirts = tmin)

# CHIRTS tmax Import Zambia -----------------------------------------------

tmax_files <- list.files(here("data", "chirts"), pattern = "tmax", full.names = TRUE)

nc <- nc_open(tmax_files[1])
stopifnot(min(nc$dim$X$vals) < min(temp_metadata$longitude))
stopifnot(max(nc$dim$X$vals) > max(temp_metadata$longitude))
stopifnot(min(nc$dim$Y$vals) < min(temp_metadata$latitude))
stopifnot(max(nc$dim$Y$vals) > max(temp_metadata$latitude))
xs <- nc$dim$X$vals
ys <- nc$dim$Y$vals
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
print(nc$dim$`T`$units)
nc_close(nc)

tmax_dfs <- list()
pb <- txtProgressBar(min = 0, max = length(tmax_files) * nrow(xy_extract), style = 3)
count <- 0
for(f in tmax_files) {
  for(i in seq_len(nrow(xy_extract))) {
    df <- tidync(f) %>%
      hyper_filter(X = X == xy_extract[i, 1], 
                   Y = Y == xy_extract[i, 2]) %>%
      hyper_tibble()
    df$station <- xy_extract$station[i]
    df$req_longitude <- xy_extract$req_longitude[i]
    df$req_latitude <- xy_extract$req_latitude[i]
    df$dist <- xy_extract$dist[i]
    tmax_dfs[[length(tmax_dfs) + 1]] <- df
    count <- count + 1
    setTxtProgressBar(pb, count)
  }
}
chirts_tmax <- bind_rows(tmax_dfs) %>%
  group_by(station) %>%
  mutate(date = as.Date(T, origin = structure(-2440588, class = "Date"))) %>%
  select(station, date, tmax_chirts = tmax)

chirts_zambia <- full_join(chirts_tmax, chirts_tmin, by = c("station", "date"))

saveRDS(chirts_zambia, here("data", "station", "cleaned", "temperature", "zambia_chirts.RDS"))

# CHIRTS point import -----------------------------------------------------

# all_files <- list.files("D:/data/chirts", full.names = TRUE)
all_files <- list.files(here("data", "chirts"), full.names = TRUE)
# zm_files <- list.files("D:/data/chirts", pattern = "zambia", full.names = TRUE)
zm_files <- list.files(here("data", "chirts"), pattern = "zambia", full.names = TRUE)
point_files <- setdiff(all_files, zm_files)
countries <- c("Tanzania", "Ghana", "Ghana", "Ghana", "Kenya", "Niger")
stations <- c("Dodoma", "Saltpond", "Tamale", "Wa", "Kisumu", "Sadore")
df_list <- vector("list", length(stations))
for (s in seq_along(stations)) {
  s_files <- list.files("D:/data/chirts", pattern = stations[s], full.names = TRUE)
  # s_files <- list.files(here("data", "chirts"), pattern = stations[s], full.names = TRUE)
  tmax_file <- s_files[grepl("tmax", s_files)]
  tmax_df <- tidync(tmax_file) %>%
    hyper_tibble()
  tmax_df$country <- countries[s]
  tmax_df$station <- stations[s]
  tmin_file <- s_files[grepl("tmin", s_files)]
  tmin_df <- tidync(tmin_file) %>%
    hyper_tibble()
  tmin_df$country <- countries[s]
  tmin_df$station <- stations[s]
  df <- full_join(tmin_df, tmax_df, by = c("country", "station", "X", "Y", "T"))
  df <- df %>% 
    dplyr::mutate(date = as.Date(T, origin = structure(-2440588, class = "Date"))) %>%
    dplyr::select(country, station, date, tmax_chirts = tmax, tmin_chirts = tmin)
  df_list[[s]] <- df
  req_lon <- temp_metadata$longitude[temp_metadata$station == stations[s]]
  req_lat <- temp_metadata$latitude[temp_metadata$station == stations[s]]
  dist = sp::spDistsN1(matrix(c(first(tmax_df$X), first(tmax_df$Y)), ncol = 2), 
                       c(req_lon, req_lat))
  locs_chirts <- locs_chirts %>% 
    add_row(Var1 = first(tmax_df$X), Var2 = first(tmax_df$Y),
            station = stations[s], req_longitude = req_lon,
            req_latitude = req_lat, dist = dist)
}
df_all <- bind_rows(df_list)

saveRDS(locs_chirts, here("data", "temp_locations_chirts.RDS"))

saveRDS(df_all, here("data", "station", "cleaned", "temperature", "africa_chirts.RDS"))
