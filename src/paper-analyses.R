library(here)
library(ggplot2)
library(lubridate)
library(reshape2)
library(viridis)
library(RColorBrewer)
library(tidyr)
library(hydroGOF)
library(stringr)
library(knitr)
library(kableExtra)
library(rnaturalearthdata)
library(rnaturalearth)
library(ggrepel)
library(sp)
library(tibble)
library(verification)
library(purrr)
library(sf)
library(ggspatial)
library(raster)
library(broom)
library(RcppRoll)
library(dplyr)
library(openair)

# Setup -------------------------------------------------------------------

source(here("src", "helper_funs.R"))

stations <- c("Sadore", "Wa", "Tamale", "Saltpond", "Kisumu", "Dodoma", "Mpika", "Livingstone")

tm <- readRDS(here("data", "temperature_1979_qc.RDS"))
daily_rain <- readRDS(here("data", "stations_all.RDS"))
daily_rain <- daily_rain %>%
  select(country, station, date, rain) %>%
  mutate(rainday = rain > 0.05)

tm_chirts_africa <- readRDS(here("data", "africa_chirts.RDS"))
tm_chirts_zambia <- readRDS(here("data", "zambia_chirts.RDS"))
tm_chirts_zambia$country <- "Zambia"
tm_chirts <- bind_rows(tm_chirts_africa, tm_chirts_zambia)
tm_chirts <- tm_chirts %>%
  filter(station %in% stations)

tm <- left_join(tm, tm_chirts, by = c("country", "station", "date"))

tm_era5 <- readRDS(here("data", "temp_gridded.RDS"))

tm_era5 <- tm_era5 %>% 
  select(country, station, date, tmax_era5, tmin_era5, tmax_era5land, tmin_era5land)

tm <- left_join(tm, tm_era5, by = c("country", "station", "date"))

tm$station <- factor(tm$station, levels = stations)

tm <- tm %>%
  mutate(year = lubridate::year(date),
         month = factor(lubridate::month(date)))

# Filter all data to range of CHIRTS
tm <- tm %>%
  filter(date >= as.Date("1983/01/01"), date <= as.Date("2016/12/31"))

tm <- left_join(tm, daily_rain, by = c("country", "station", "date"))

station_metadata <- readRDS(here("data", "stations_metadata.RDS"))
temp_metadata <- station_metadata %>%
  filter(station %in% stations)

by_station <- tm %>%
  group_by(country, station) %>%
  filter(!is.na(tmin) & !is.na(tmax)) %>%
  summarise(first_date = first(date),
            first_year = year(first(date)),
            last_date = last(date),
            last_year = year(last(date)))

tm <- left_join(tm, by_station, by = c("country", "station"))
tm <- tm %>% filter(date <= last_date)

by_station2 <- tm %>%
  group_by(country, station) %>%
  summarise(complete_days = 100 * sum(!is.na(tmin) & !is.na(tmin))/n())

tm <- tm %>%
  select(country, station, date, year, month, tmax, tmin, rain, rainday,
         tmax_era5, tmin_era5, tmax_era5land, tmin_era5land, tmax_chirts, tmin_chirts)

temp_metadata <- left_join(temp_metadata, by_station, by = c("country", "station"))
temp_metadata <- left_join(temp_metadata, by_station2, by = c("country", "station"))

temp_metadata <- temp_metadata %>% arrange(-latitude)

tm$country_station <- paste(tm$country, tm$station, sep = " - ")
tm$country_station <- factor(tm$country_station, 
                             levels = paste(temp_metadata$country, temp_metadata$station, 
                                            sep = " - "))

# Products ----------------------------------------------------------------

d <- data.frame(Product = c("ERA5", "ERA5-Land", "CHIRTS"),
                `Spatial Resolution` = c(0.25, 0.1, 0.05),
                `Temporal Resolution` = c("Hourly", "Hourly", "Daily"),
                `Data Availability` = c("1979 - Present (Preliminary version from 1950)",
                                        "1981 - Present (Version from 1950 expected in 2021)",
                                        "1983 - 2016"),
                Coverage = c("Global", "Global", "Global"),
                Method = c("Reanalysis", "Reanalysis", "Merged Station, Satellite & Reanalysis"),
                check.names = FALSE)

write.csv(d, here("results", "table_products.csv"), row.names = FALSE)

# Stations ----------------------------------------------------------------

st_details <- temp_metadata %>% 
  dplyr::transmute(Country = country,
                   Station = station,
                   Latitude = round(latitude, 2),
                   Longitude = round(longitude, 2),
                   `Data Range` = paste(first_date, "-", last_date),
                   `Complete Days (%)` = round(complete_days, 1)) %>%
  arrange(Country, -Latitude)

write.csv(st_details, here("results", "table_station_details.csv"), row.names = FALSE)

sf_af <- ne_countries(returnclass = "sf", continent = "africa")
ggplot(sf_af) + 
  geom_sf(fill = "antiquewhite", colour = "grey40") +
  geom_point(data = temp_metadata, aes(x = longitude, y = latitude)) +
  geom_label_repel(data = temp_metadata, aes(x = longitude, y = latitude, label = station),
                   size = 3.3) +
  theme_bw() +
  theme(panel.grid.major = element_line(color = gray(0.7), linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "aliceblue")) +
  labs(x = "Longitude", y = "Latitude")
ggsave(here("results", "map_stations.png"), width = 6, height = 7)

tm_stack <- pivot_longer(tm, cols = c("tmin", "tmax"), 
                         names_to = "element", values_to = "value")
ggplot(tm_stack, aes(x = date, y = element, fill = factor(is.na(value)))) +
  geom_tile() +
  geom_hline(yintercept = seq(0.5, by = 1, length.out = length(unique(tm_stack$element)) + 1)) +
  geom_vline(data = temp_metadata, aes(xintercept = first_date)) +
  geom_vline(data = temp_metadata, aes(xintercept = last_date)) +
  scale_x_date(name = "Date", date_breaks = "2 years", date_labels = "%Y") +
  scale_fill_discrete(name = "", labels = c("Present", "Missing"), type = c("grey90", "red3")) +
  facet_wrap(vars(country, station), ncol = 1, strip.position = "right") +
  labs(title = "Station Data Inventory",
       y = "Element") +
  theme(strip.text = element_text(size = 10),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5))
ggsave(here("results", "stations_inventory.png"), width = 12, height = 6)


# Daily Analysis ----------------------------------------------------------

tm_long <- tm %>% 
  pivot_longer(cols = tmax_era5:tmin_chirts, names_to = c(".value", "product"),
               names_sep = "(.)_(.)")
names(tm_long)[ncol(tm_long) - 1] <- "tmax_product"
names(tm_long)[ncol(tm_long)] <- "tmin_product"
tm_long$station <- factor(tm_long$station, levels = stations)
tm_long$product <- recode(tm_long$product, 
                          ra5 = "ERA5", ra5land = "ERA5 Land", hirts = "CHIRTS")
tm_long$product <- factor(tm_long$product, levels = c("CHIRTS", "ERA5", "ERA5 Land"))
tm_long$tmax_bias <- tm_long$tmax_product - tm_long$tmax
tm_long$tmin_bias <- tm_long$tmin_product - tm_long$tmin

by_station <- tm_long %>%
  group_by(country_station, country, station, product) %>%
  summarise(cor_tmin = cor(tmin_product, tmin, use = "na.or.complete"),
            cor_tmax = cor(tmax_product, tmax, use = "na.or.complete"),
            me_tmin = hydroGOF::me(tmin_product, tmin),
            me_tmax = hydroGOF::me(tmax_product, tmax),
            rmse_tmin = hydroGOF::rmse(tmin_product, tmin),
            rmse_tmax = hydroGOF::rmse(tmax_product, tmax),
            sd_tmax_bias = sd(tmax_bias, na.rm = TRUE),
            sd_tmin_bias = sd(tmin_bias, na.rm = TRUE)
            #rsd_tmin = hydroGOF::rSD(tmin_product, tmin),
            #rsd_tmax = hydroGOF::rSD(tmax_product, tmax)
  )

by_station_rsd <- tm_long %>%
  group_by(country_station, country, station, product) %>%
  filter(station != "Saltpond" | product != "ERA5 Land") %>%
  summarise(rsd_tmin = hydroGOF::rSD(tmin_product, tmin),
            rsd_tmax = hydroGOF::rSD(tmax_product, tmax)
  )

by_station %>% pivot_wider(id_cols = station, names_from = product, values_from = cor_tmin)
by_station %>% pivot_wider(id_cols = station, names_from = product, values_from = cor_tmax)

# To get an overall correlation value that accounts for seasonality
# calculate long term monthly anomolies and correlate these.
# Similar to correlations by month, but with an overall value.
# tm_long %>% 
#   group_by(country_station, country, station, product, month) %>%
#   mutate(tmin_anom = tmin - mean(tmin, na.rm = TRUE),
#          tmax_anom = tmax - mean(tmax, na.rm = TRUE),
#          tmin_product_anom = tmin_product - mean(tmin_product, na.rm = TRUE),
#          tmax_product_anom = tmax_product - mean(tmax_product, na.rm = TRUE)) %>%
#   group_by(country_station, country, station, product) %>%
#   summarise(cor_tmin = cor(tmin_product_anom, tmin_anom, use = "na.or.complete"),
#             cor_tmax = cor(tmax_product_anom, tmax_anom, use = "na.or.complete")) %>%
#   pivot_wider(id_cols = station, names_from = product, values_from = cor_tmax)

by_station %>% pivot_wider(id_cols = station, names_from = product, values_from = me_tmin)
by_station %>% pivot_wider(id_cols = station, names_from = product, values_from = me_tmax)

by_station %>% pivot_wider(id_cols = station, names_from = product, values_from = rmse_tmin)
by_station %>% pivot_wider(id_cols = station, names_from = product, values_from = rmse_tmax)

by_station_rsd %>% pivot_wider(id_cols = station, names_from = product, values_from = rsd_tmin)
by_station_rsd %>% pivot_wider(id_cols = station, names_from = product, values_from = rsd_tmax)

by_station %>% pivot_wider(id_cols = station, names_from = product, values_from = sd_tmin_bias)
by_station %>% pivot_wider(id_cols = station, names_from = product, values_from = sd_tmax_bias)

# Demonstrate systematic tmin bias at Sadore with scatter plots
ggplot(tm_long %>% filter(station == "Sadore" & product != "ERA5 Land"), 
       aes(x = tmin, y = tmin_product)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, colour = "pink") +
  coord_equal() +
  facet_grid(vars(product), vars(month))

by_station_month <- tm_long %>%
  group_by(country_station, country, station, product, month) %>%
  summarise(cor_tmin = cor(tmin_product, tmin, use = "na.or.complete"),
            cor_tmax = cor(tmax_product, tmax, use = "na.or.complete"),
            me_tmin = hydroGOF::me(tmin_product, tmin),
            me_tmax = hydroGOF::me(tmax_product, tmax),
            rmse_tmin = hydroGOF::rmse(tmin_product, tmin),
            rmse_tmax = hydroGOF::rmse(tmax_product, tmax),
            sd_tmax_bias = sd(tmax_bias, na.rm = TRUE),
            sd_tmin_bias = sd(tmin_bias, na.rm = TRUE)
            #rsd_tmin = hydroGOF::rSD(tmin_product, tmin),
            #rsd_tmax = hydroGOF::rSD(tmax_product, tmax)
  )

by_station_month_rsd <- tm_long %>%
  group_by(country_station, country, station, product, month) %>%
  filter(station != "Saltpond" | product != "ERA5 Land") %>%
  summarise(rsd_tmin = hydroGOF::rSD(tmin_product, tmin),
            rsd_tmax = hydroGOF::rSD(tmax_product, tmax),
            me_tmin = hydroGOF::me(tmin_product, tmin),
            me_tmax = hydroGOF::me(tmax_product, tmax),
            me_tmin_lower = t.test(tmin_product - tmin)$conf.int[1],
            me_tmin_upper = t.test(tmin_product - tmin)$conf.int[2],
            me_tmax_lower = t.test(tmax_product - tmax)$conf.int[1],
            me_tmax_upper = t.test(tmax_product - tmax)$conf.int[2],
  )

by_station_long_month <- tm %>%
  group_by(country_station, country, station, month, year) %>%
  summarise(t_rain = sum(naif_nmin(rain, 25))) %>%
  summarise(mean_month = mean(t_rain, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(mean_month_scale = mean_month/max(mean_month))

max_mean_month <- max(by_station_long_month$mean_month)

# Daily correlations by month with long term monthly rainfall bars
# Change tmin/tmax to get both graphs
ggplot(by_station_month, aes(x = as.numeric(month), y = cor_tmin, colour = product)) +
  geom_col(data = by_station_long_month, 
           aes(x = as.numeric(month), y = mean_month_scale), inherit.aes = FALSE,
           fill = "lightsteelblue1", colour = "black", alpha = 0.5) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c25[1:12]) +
  scale_x_continuous(name = "Month", breaks = 1:12) +
  scale_y_continuous(name = "Correlation Coefficient (r)", limits = c(0, 1), 
                     breaks = seq(0, 1, 0.25), 
                     sec.axis = sec_axis(trans = ~ . * max_mean_month, 
                                         name = "Total Rainfall (mm)")) +
  labs(colour = "Product") +
  facet_wrap(vars(country_station), scales = "free_x", ncol = 4) +
  ggtitle("Correlation of daily minimum temperatures by month")

ggsave(here("results", "daily_tmin_correlation_month.png"), width = 12, height = 6)

# Mean bias by month
# Change tmin/tmax to get both graphs
ggplot(by_station_month, aes(x = as.numeric(month), y = me_tmax, colour = product)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, colour = "black") +
  scale_color_manual(values = c25[1:12]) +
  scale_x_continuous(name = "Month", breaks = 1:12) +
  scale_y_continuous(name = "Mean Bias", breaks = seq(-6, 6, 2)) +
  labs(colour = "Product") +
  facet_wrap(vars(country_station), scales = "free_x", ncol = 4) +
  ggtitle("Mean bias of daily maximum temperatures by month")

# RMSE by month
# Change tmin/tmax to get both graphs
ggplot(by_station_month, aes(x = as.numeric(month), y = rmse_tmax, colour = product)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c25[1:12]) +
  scale_x_continuous(name = "Month", breaks = 1:12) +
  scale_y_continuous(name = "Mean Bias", breaks = seq(0, 10, 2)) +
  labs(colour = "Product") +
  facet_wrap(vars(country_station), scales = "free_x", ncol = 4) +
  ggtitle("RMSE of daily maximum temperatures by month")

# Comparisons also by rainday - for reporting but not including
by_station_month_rain <- tm_long %>%
  group_by(station, product, month, rainday) %>%
  summarise(cor_tmin = cor(tmin_product, tmin, use = "na.or.complete"),
            cor_tmax = cor(tmax_product, tmax, use = "na.or.complete"))

# Daily correlations by month and by rain day
ggplot(by_station_month_rain, aes(x = as.numeric(month), y = cor_tmin, 
                                  colour = rainday)) +
  geom_col(data = by_station_long_month, 
           aes(x = as.numeric(month), y = mean_month_scale), inherit.aes = FALSE,
           fill = "lightsteelblue1", colour = "black", alpha = 0.5) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c25[1:12]) +
  scale_x_continuous(breaks = 1:12) +
  scale_y_continuous(limits = c(0, 1)) +
  facet_grid(product~station) +
  ggtitle("Correlation of daily tmin by month")

# # Monthly Analysis --------------------------------------------------------
# 
# Follows suggestion from WMO Guidelines on normal calculations
# https://library.wmo.int/doc_num.php?explnum_id=4166
# month_means <- tm_long %>%
#   group_by(country_station, country, station, product, year, month) %>%
#   summarise(mean_tmin = mean(naif(tmin, 10, 4)),
#             mean_tmin_product = mean(naif(tmin_product, 10, 4)),
#             mean_tmax = mean(naif(tmax, 10, 4)),
#             mean_tmax_product = mean(naif(tmax_product, 10, 4))
#             ) %>%
#   mutate(mean_tmin_bias = mean_tmin_product - mean_tmin,
#          mean_tmax_bias = mean_tmax_product - mean_tmax)
# 
# month_mean_stats <- month_means %>%
#   group_by(country_station, country, station, product) %>%
#   summarise(cor_tmin = cor(mean_tmin_product, mean_tmin, use = "na.or.complete"),
#             cor_tmax = cor(mean_tmax_product, mean_tmax, use = "na.or.complete"),
#             me_tmin = hydroGOF::me(mean_tmin_product, mean_tmin),
#             me_tmax = hydroGOF::me(mean_tmax_product, mean_tmax),
#             rmse_tmin = hydroGOF::rmse(mean_tmin_product, mean_tmin),
#             rmse_tmax = hydroGOF::rmse(mean_tmax_product, mean_tmax),
#             sd_tmax_bias = sd(mean_tmax_bias, na.rm = TRUE),
#             sd_tmin_bias = sd(mean_tmin_bias, na.rm = TRUE)
#             #rsd_tmin = hydroGOF::rSD(mean_tmin_product, mean_tmin),
#             #rsd_tmax = hydroGOF::rSD(mean_tmax_product, mean_tmax)
#   )
# 
# month_mean_rsd <- month_means %>%
#   group_by(country_station, country, station, product) %>%
#   filter(station != "Saltpond" | product != "ERA5 Land") %>%
#   summarise(rsd_tmin = hydroGOF::rSD(mean_tmin_product, mean_tmin),
#             rsd_tmax = hydroGOF::rSD(mean_tmax_product, mean_tmax)
#   )
# 
# month_mean_stats %>% pivot_wider(id_cols = c(station), names_from = product, values_from = cor_tmin)
# 
# ggplot(month_means, aes(x = mean_tmin, y = mean_tmin_product, colour = product)) +
#   geom_point(alpha = 0.1) +
#   geom_abline(intercept = 0, slope = 1) +
#   coord_equal() +
#   ggtitle("Monthly mean tmax correlation") +
#   facet_grid(station~product)


# Yearly Analysis ---------------------------------------------------------

# Use strict criteria as yearly values could be highly influenced by consecutive missing
year_means <- tm_long %>%
  group_by(country_station, country, station, product, year) %>%
  summarise(mean_tmin = mean(naif(tmin, 27, 20)),
            mean_tmin_product = mean(naif(tmin_product, 27, 20)),
            mean_tmax = mean(naif(tmax, 27, 20)),
            mean_tmax_product = mean(naif(tmax_product, 27, 20)),
            max_tmax = max(naif(tmax, 27, 20)),
            max_tmax_product = max(naif(tmax_product, 27, 20)),
            ) %>%
  group_by(country_station, country, station, product) %>%
  mutate(mean_tmin_overall = mean(mean_tmin, na.rm = TRUE),
         mean_tmax_overall = mean(mean_tmax, na.rm = TRUE),
         mean_tmin_product_overall = mean(mean_tmin_product, na.rm = TRUE),
         mean_tmax_product_overall = mean(mean_tmax_product, na.rm = TRUE),
         mean_tmin_anom = mean_tmin - mean_tmin_overall,
         mean_tmax_anom = mean_tmax - mean_tmax_overall,
         mean_tmin_product_anom = mean_tmin_product - mean_tmin_product_overall,
         mean_tmax_product_anom = mean_tmax_product - mean_tmax_product_overall
         )

year_mean_stats <- year_means %>%
  group_by(country_station, country, station, product) %>%
  summarise(cor_tmin = cor(mean_tmin_product, mean_tmin, use = "na.or.complete"),
            cor_tmax = cor(mean_tmax_product, mean_tmax, use = "na.or.complete"),
            sd_tmin = sd(mean_tmin, na.rm = TRUE),
            sd_tmax = sd(mean_tmax, na.rm = TRUE),
            me_tmin = hydroGOF::me(mean_tmin_product, mean_tmin),
            me_tmax = hydroGOF::me(mean_tmax_product, mean_tmax),
            rmse_tmin = hydroGOF::rmse(mean_tmin_product, mean_tmin),
            rmse_tmax = hydroGOF::rmse(mean_tmax_product, mean_tmax),
            #rsd_tmin = hydroGOF::rSD(mean_tmin_product, mean_tmin),
            #rsd_tmax = hydroGOF::rSD(mean_tmax_product, mean_tmax)
  )

year_mean_stats %>% pivot_wider(id_cols = station, names_from = product, values_from = cor_tmin)
year_mean_stats %>% pivot_wider(id_cols = station, names_from = product, values_from = sd_tmin)

year_mean_stats %>% pivot_wider(id_cols = station, names_from = product, values_from = me_tmin)
year_mean_stats %>% pivot_wider(id_cols = station, names_from = product, values_from = cor_tmax)

ggplot(year_means, aes(x = year, y = mean_tmin, colour = "station")) +
  geom_line() +
  geom_line(aes(y = mean_tmin_product, colour = product)) +
  scale_color_manual(values = c(c25[1:3], "black")) +
  facet_wrap(vars(station)) +
  ggtitle("Yearly mean tmin")

ggplot(year_means, aes(x = year, y = mean_tmax, colour = "station")) +
  geom_line() +
  geom_line(aes(y = mean_tmax_product, colour = product)) +
  scale_color_manual(values = c(c25[1:3], "black")) +
  facet_wrap(vars(station)) +
  ggtitle("Yearly mean tmax")

year_nest <- year_means %>%
  group_by(country_station, country, station, product) %>%
  nest() %>%
  mutate(lm_tmin = purrr::map(data, possibly(lm, otherwise = NULL), 
                              formula = mean_tmin ~ year),
         lm_tmax = purrr::map(data, possibly(lm, otherwise = NULL), 
                              formula = mean_tmax ~ year),
         lm_tmin_product = purrr::map(data, possibly(lm, otherwise = NULL),
                                      formula = mean_tmin_product ~ year),
         lm_tmax_product = purrr::map(data, possibly(lm, otherwise = NULL),
                                      formula = mean_tmax_product ~ year),
         slope_tmin = purrr::map_dbl(lm_tmin, 
                                    ~ifelse(is.null(.x), NA_real_,
                                            purrr::pluck(.x, "coefficients", "year"))),
         slope_tmax = purrr::map_dbl(lm_tmax, 
                                         ~ifelse(is.null(.x), NA_real_,
                                                 purrr::pluck(.x, "coefficients", "year"))),
         slope_tmin_product = purrr::map_dbl(lm_tmin_product,
                                                 ~ifelse(is.null(.x), NA_real_,
                                                         purrr::pluck(.x, "coefficients", "year"))),
         slope_tmax_product = purrr::map_dbl(lm_tmax_product,
                                                 ~ifelse(is.null(.x), NA_real_,
                                                         purrr::pluck(.x, "coefficients", "year"))),
         p_tmin = purrr::map_dbl(lm_tmin, ~ifelse(is.null(.x), NA_real_, summary(.x)$coefficients[2, 4])),
         p_tmax = purrr::map_dbl(lm_tmax, ~ifelse(is.null(.x), NA_real_, summary(.x)$coefficients[2, 4])),
         p_tmin_product = purrr::map_dbl(lm_tmin_product, ~ifelse(is.null(.x), NA_real_, summary(.x)$coefficients[2, 4])),
         p_tmax_product = purrr::map_dbl(lm_tmax_product, ~ifelse(is.null(.x), NA_real_, summary(.x)$coefficients[2, 4])),
  )

year_nest %>% filter(product == "CHIRTS") %>% ungroup() %>% transmute(station, slope_tmin, p = p_tmin < 0.05, slope_tmin_product, p_tmin_product < 0.05)
year_nest %>% filter(product == "ERA5") %>% ungroup() %>% transmute(station, slope_tmin, p = p_tmin < 0.05, slope_tmin_product, p_tmin_product < 0.05)
year_nest %>% filter(product == "ERA5 Land") %>% ungroup() %>% transmute(station, slope_tmin, p = p_tmin < 0.05, slope_tmin_product, p_tmin_product < 0.05)

year_nest %>% filter(product == "CHIRTS") %>% ungroup() %>% transmute(station, slope_tmax, p = p_tmin < 0.05, slope_tmax_product, p_tmax_product < 0.05)
year_nest %>% filter(product == "ERA5") %>% ungroup() %>% transmute(station, slope_tmax, p = p_tmin < 0.05, slope_tmax_product, p_tmax_product < 0.05)
year_nest %>% filter(product == "ERA5 Land") %>% ungroup() %>% transmute(station, slope_tmax, p = p_tmin < 0.05, slope_tmax_product, p_tmax_product < 0.05)

# Extremes ----------------------------------------------------------------

year_means$max_tmax[year_means$max_tmax == max(year_means$max_tmax, na.rm = TRUE)] <- NA
year_max_stats <- year_means %>%
  group_by(country_station, country, station, product) %>%
  summarise(me_max_tmax = hydroGOF::me(max_tmax_product, max_tmax)
            )

year_max_stats %>% pivot_wider(id_cols = station, names_from = product, values_from = me_max_tmax)
