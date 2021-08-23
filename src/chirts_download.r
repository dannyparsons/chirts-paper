library(here)
library(dplyr)
library(lubridate)

save_path <- function(file) {
  here("data", "chirts", file)
}

chirts_path <- "https://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRTS/.v1.0/.daily/.global/.0p05/"

chirts_path_t <- paste0(chirts_path, "T/(1%20Jan%201981)/(31%20Dec%202016)/RANGEEDGES/")

add_chirts_x <- function(path, lower, upper) {
  paste0(path, "X", "/",
        "(", ifelse(lower < 0, paste0(abs(lower), "W"), paste0(lower, "E")), ")", "/",
        "(", ifelse(upper < 0, paste0(abs(upper), "W"), paste0(upper, "E")), ")", "/",
        "RANGEEDGES", "/"
  )
}

add_chirts_x_point <- function(path, value) {
  paste0(path, "X", "/", "%28", ifelse(value < 0, paste0(abs(value), "W"), paste0(value, "E")), "%29", "VALUES", "/")
}

add_chirts_y <- function(path, lower, upper) {
  paste0(path, "Y", "/",
         "(", ifelse(lower < 0, paste0(abs(lower), "S"), paste0(lower, "N")), ")", "/",
         "(", ifelse(upper < 0, paste0(abs(upper), "S"), paste0(upper, "N")), ")", "/",
         "RANGEEDGES", "/"
  )
}

add_chirts_y_point <- function(path, value) {
  paste0(path, "Y", "/", "%28", ifelse(value < 0, paste0(abs(value), "S"), paste0(value, "N")), "%29", "VALUES", "/")
}

add_chirts_t <- function(path, lower, upper) {
  paste0(path, "T", "/",
         "(", lubridate::day(lower), "%20", lubridate::month(lower, label = TRUE), 
         "%20", lubridate::year(lower), ")", "/",
         "(", lubridate::day(upper), "%20", lubridate::month(upper, label = TRUE), 
         "%20", lubridate::year(upper), ")", "/",
         "RANGEEDGES", "/"
  )
}

add_chirts_nc <- function(path) {
  paste0(path, "data.nc")
}

# Zambia ------------------------------------------------------------------

d_lower <- as.Date("1981/01/01")
limit <- as.Date("2016/12/31")
while(d_lower < limit) {
  d_upper <- d_lower
  lubridate::year(d_upper) <- lubridate::year(d_lower) + 10
  if(d_upper > limit) d_upper <- limit
  for (i in c("tmin", "tmax")) {
    path_zambia <- paste0(chirts_path, ".", i, "/") %>%
      add_chirts_x(21.8, 34) %>%
      add_chirts_y(-18.2, -8) %>%
      add_chirts_t(d_lower, d_upper) %>%
      add_chirts_nc()
    print(d_lower)
    print(d_upper)
    download.file(path_zambia, save_path(paste0("chirts_zambia_", i, "_", d_lower, "_", d_upper, ".nc")), method = "curl", cacheOK = FALSE)
  }
  d_lower <- d_upper + 1
}


# Station Points ----------------------------------------------------------

station_metadata <- readRDS(here("data", "station", "processed", "stations_metadata.RDS"))

station_metadata <- station_metadata %>%
  filter(station %in% c("Dodoma", "Saltpond", "Tamale", "Wa", "Kisumu", "Sadore"))

chirts_point_paths_tmin <- paste0(chirts_path, ".", "tmin", "/") %>% 
  add_chirts_x_point(station_metadata$longitude) %>%
  add_chirts_y_point(station_metadata$latitude) %>%
  add_chirts_nc()
d_lower <- "1983-01-01"
d_upper <- "2016-12-31"
for (i in c("tmin", "tmax")) {
  print(i)
  paths <- paste0(chirts_path, ".", i, "/") %>% 
    add_chirts_x_point(station_metadata$longitude) %>%
    add_chirts_y_point(station_metadata$latitude) %>%
    add_chirts_nc()
  for (j in seq_along(paths)) {
    print(station_metadata$station[j])
    download.file(paths[j], save_path(paste0("chirts_", station_metadata$station[j], "_", i, "_", d_lower, "_", d_upper, ".nc")), method = "curl", cacheOK = FALSE)
    
  }
}







