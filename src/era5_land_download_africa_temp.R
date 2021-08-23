library(lubridate)
library(ecmwfr)
library(here)

user <- wf_set_key(user = "255",
                   key = "245a05d4-57b8-43c4-afb7-3da2a5010cff",
                   service = "cds")

# area: N/W/S/E
era5_download_area <- function(area, name, variable = "total_precipitation", dates) {
  if(missing(dates)) dates <- seq(as.Date("1981/01/1"), as.Date("2020/05/30"), by = "1 month")
  pb <- txtProgressBar(min = 0, max = length(dates), style = 3)
  for(i in seq_along(dates)) {
    cat("\n")
    request <- list(
      dataset = "reanalysis-era5-land",
      product_type = "reanalysis",
      variable = variable,
      year = as.character(lubridate::year(dates[i])),
      month = as.character(lubridate::month(dates[i])),
      day = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"),
      time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
      format = "netcdf",
      area = area,
      target = paste0(name, "-", variable, "-", lubridate::year(dates[i]), 
                      sprintf("%02d", lubridate::month(dates[i])), ".nc")
    )
    try(ncfile <- wf_request(user = user, request = request,
                             transfer = TRUE, path = "D:/data/era5_land",
                             time_out = 3 * 3600))
    setTxtProgressBar(pb, i)
  }
}

# Africa
files <- list.files("D:/data/era5_land", pattern = "africa-2m_temperature")
mm <- substr(files, 23, 28)
dates_done <- as.Date(paste(mm, 1), format = "%Y%m %d")
dates_all <- seq(as.Date("1981/01/1"), as.Date("2020/05/30"), by = "1 month")
dates_remain <- as.Date(dplyr::setdiff(dates_all, dates_done), origin = "1970-01-01")
while (length(dates_remain) > 0) {
  era5_download_area(area = c(38, -20, -35, 52), name = "africa", variable = "2m_temperature", dates = dates_remain)
  files <- list.files("D:/data/era5_land", pattern = "africa-2m_temperature")
  mm <- substr(files, 23, 28)
  dates_done <- as.Date(paste(mm, 1), format = "%Y%m %d")
  dates_all <- seq(as.Date("1981/01/1"), as.Date("2020/05/30"), by = "1 month")
  dates_remain <- as.Date(dplyr::setdiff(dates_all, dates_done), origin = "1970-01-01")
}

# Barbados
#era5_download_area(area = c(25.4, -85.2, 9.89, -59.3), name = "caribbean")

# Ghana temperature
# era5_download_area(area = c(11, -3.3, 4.6, 1.8), 
#                    name = "ghana", variable = "2m_temperature")

# Africa temperature
# era5_download_area(area = c(38, -20, -35, 52), 
#                    name = "africa", variable = "2m_temperature")

# # Zambia 3 stations temperature
# era5_download_area(area = c(-10.5, 25.5, -18.2, 29.2), 
#                    name = "zambia", variable = "2m_temperature")
# 
# # Dodoma temperature
# era5_download_area(area = c(-5.5, 35, -6.5, 36.1), 
#                    name = "dodoma", variable = "2m_temperature")
# 
# # Koundara temperature
# era5_download_area(area = c(13, -13.8, 12, -12.5), 
#                    name = "koundara", variable = "2m_temperature")
# 
# # Kisumu temperature
# era5_download_area(area = c(1, 34, -1, 35.3), 
#                    name = "kisumu", variable = "2m_temperature")
# 
# # Sadore temperature
# era5_download_area(area = c(14, 1.5, 12.7, 3), 
#                    name = "sadore", variable = "2m_temperature")
# 
# # Mali temperature
# era5_download_area(area = c(13.2, -8.7, 12, -7.5), 
#                    name = "samanko", variable = "2m_temperature")
# 
# # Husbands temperature
# era5_download_area(area = c(13.8, -60.2, 12.5, -59.1),
#                    name = "husbands", variable = "2m_temperature")
