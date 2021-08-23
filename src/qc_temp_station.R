library(here)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(purrr)

source(here("src", "helper_funs.R"))

daily_temp <- readRDS(here("data", "stations_temperature_1979.RDS"))

daily_temp <- daily_temp %>% 
  arrange(country, station, date) %>%
  group_by(country, station) %>%
  mutate(min_date = date[min(which(!is.na(tmin) | !is.na(tmax)))],
         max_date = date[max(which(!is.na(tmin) | !is.na(tmax)))],
         country_station = paste(country, station, sep = ".")) %>%
  filter(date >= min_date & date <= max_date) %>%
  dplyr::select(-c(min_date, max_date))

daily_temp <- daily_temp %>% 
  mutate(year = year(date), month = month(date), day = day(date))

# Fill Date gaps
dates_list <- list()
for(s in unique(daily_temp$station)) {
  
  dates <- seq(min((daily_temp %>% filter(station == s))$date), 
               max((daily_temp %>% filter(station == s))$date),
               by = 1)
  dd <- data.frame(country = first((daily_temp %>% filter(station == s))$country),
                   station = s, date = dates)
  dates_list[[length(dates_list) + 1]] <- dd
}
date_df <- bind_rows(dates_list)

nr <- nrow(date_df)
if(nrow(daily_temp) < nr) {
  print(paste("Filling", nr - nrow(daily_temp), "rows"))
  daily_temp <- full_join(date_df, daily_temp, by = c("country", "station", "date"))
  daily_temp <- daily_temp %>%
    mutate(year = year(date), month = factor(month(date)))
}

# Inventory plot
daily_temp_stack <- pivot_longer(daily_temp, cols = c("tmin", "tmax"), names_to = "element", values_to = "value")
g <- ggplot(daily_temp_stack, aes(x = date, y = element, fill = !is.na(value))) +
  geom_tile() +
  geom_hline(yintercept = seq(0.5, by = 1, length.out = length(unique(daily_temp_stack$element)) + 1)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~country + station, ncol = 1, strip.position = "right") +
  theme(strip.text = element_text(size = 7))

ggsave(here("results", "inventory.png"), g, width = 18, height = 9)

# Very high values
vhigh <- daily_temp %>% filter(tmax > 50 | tmin > 50)
if(nrow(vhigh) > 0) View(vhigh)

# Very low tmax values
vlow <- daily_temp %>% filter(tmax < 10)
if(nrow(vlow) > 0) View(vlow)

# All look incorrect - make missing
daily_temp$tmax[daily_temp$station == "Sadore" & daily_temp$tmax < 10] <- NA

# Very low tmin values outside of Zambia
vlow <- daily_temp %>% filter(tmin < 10 & country != "Zambia")
if(nrow(vlow) > 0) View(vlow)

# View months of suspecious values

# 1.8 looks like it should be 18
View(daily_temp %>% filter(station == "Wa" & year == 1986 & month == 12))
daily_temp$tmin[daily_temp$station == "Wa" & daily_temp$date == as.Date("1986-12-17")] <- 18

# Potenially incorrect values
# Q: Are these jumps too big? Make these missing?
View(daily_temp %>% filter(station == "Sadore" & year == 1983 & month == 10))
View(daily_temp %>% filter(station == "Sadore" & year == 2001 & month == 1))

# Incorrect 0, make missing
View(daily_temp %>% filter(station == "Sadore" & year == 2001 & month == 10))
daily_temp$tmin[daily_temp$station == "Sadore" & daily_temp$date == as.Date("2001-10-07")] <- NA

# Not an unusual jump
View(daily_temp %>% filter(station == "Sadore" & year == 2008 & month == 1))

# Not unusual jumps
View(daily_temp %>% filter(station == "Dodoma" & year == 1986 & month == 6))
View(daily_temp %>% filter(station == "Dodoma" & year == 1986 & month == 7))
View(daily_temp %>% filter(station == "Dodoma" & year == 1992 & month == 7))

# monthly boxplots
nest_station <- daily_temp %>%
  group_by(country, station) %>%
  nest()
f_tmax <- function(data, name) {
  ggplot(data, aes(x = factor(month), y = tmax)) +
    geom_boxplot() +
    ggtitle(name)
}
f_tmin <- function(data, name) {
  ggplot(data, aes(x = factor(month), y = tmin)) +
    geom_boxplot() +
    ggtitle(name)
}

nest_station <- nest_station %>% mutate(month_boxs_tmax = purrr::map2(.x = data, .y = paste(country, station, sep = "."), 
                                                                 .f = f_tmax),
                                        month_boxs_tmin = purrr::map2(.x = data, .y = paste(country, station, sep = "."),
                                                                      .f = f_tmin))
# Inspect tmax values from boxplots
nest_station$month_boxs_tmax

# Q: Possible too low tmax
View(daily_temp %>% filter(station == "Saltpond" & month == 3 & tmax < 24))
View(daily_temp %>% filter(station == "Saltpond" & month == 3 & year == 1979))

# Too low tmax and tmax < tmin so make tmax missing
View(daily_temp %>% filter(station == "Kisumu" & month == 8 & tmax < 15))
daily_temp$tmax[daily_temp$station == "Kisumu" & daily_temp$date == as.Date("1993-08-23")] <- NA

View(daily_temp %>% filter(station == "Dodoma" & tmax < 17))
# Looks like tmin entered in tmax so make tmax missing
View(daily_temp %>% filter(station == "Dodoma" & month == 10 & year == 1989))
daily_temp$tmax[daily_temp$station == "Dodoma" & daily_temp$date == as.Date("1989-10-09")] <- NA
# Large tmax jump, looks incorrect make tmax missing
View(daily_temp %>% filter(station == "Dodoma" & month == 9 & year == 2004))
daily_temp$tmax[daily_temp$station == "Dodoma" & daily_temp$date == as.Date("2004-09-28")] <- NA

# Inspect tmin values from boxplots
nest_station$month_boxs_tmin

# Possibly too high tmin, jump not big enough to be sure
View(daily_temp %>% filter(station == "Wa" & tmin > 27 & month == 10))
View(daily_temp %>% filter(station == "Wa" & month == 10 & year == 1983))

# Too low tmin and too big jump, make tmin missing
View(daily_temp %>% filter(station == "Sadore" & tmin < 10 & month == 10))
View(daily_temp %>% filter(station == "Sadore" & month == 10 & year == 1983))
daily_temp$tmin[daily_temp$station == "Sadore" & daily_temp$date == as.Date("1983-10-19")] <- NA

# Possibly too low tmin
View(daily_temp %>% filter(station == "Dodoma" & tmin < 12.5 & month == 2))
View(daily_temp %>% filter(station == "Dodoma" & month == 2 & year == 1986))

View(daily_temp %>% filter(station == "Mpika" & tmin < 6 & month %in% c(1, 9, 11)))
# Too low tmin and too big jump, make tmin missing
View(daily_temp %>% filter(station == "Mpika" & month == 1 & year == 1983))
daily_temp$tmin[daily_temp$station == "Mpika" & daily_temp$date == as.Date("1983-1-10")] <- NA
View(daily_temp %>% filter(station == "Mpika" & month == 11 & year == 1986))
daily_temp$tmin[daily_temp$station == "Mpika" & daily_temp$date == as.Date("1986-11-23")] <- NA
View(daily_temp %>% filter(station == "Mpika" & month == 9 & year == 1992))
daily_temp$tmin[daily_temp$station == "Mpika" & daily_temp$date == as.Date("1992-9-15")] <- NA

# tmin > tmax values
maxmin <- daily_temp %>% filter(tmax - tmin <= 0)
if(nrow(maxmin) > 0) View(maxmin)

# tmax low and big jump, make tmax missing
View(daily_temp %>% filter(station == "Sadore" & month == 8 & year == 2006))
daily_temp$tmax[daily_temp$station == "Sadore" & daily_temp$date == as.Date("2006-8-13")] <- NA

# Either tmax or tmin incorrect but both plausable values, so make both missing
View(daily_temp %>% filter(station == "Wa" & month %in% c(8, 9) & year == 1980))
daily_temp$tmax[daily_temp$station == "Wa" & daily_temp$date == as.Date("1980-09-01")] <- NA
daily_temp$tmin[daily_temp$station == "Wa" & daily_temp$date == as.Date("1980-09-01")] <- NA

# tmax low and big jump, make tmax missing
View(daily_temp %>% filter(station == "Kisumu" & month %in% c(12, 1) & year %in% c(1994, 1995)))
daily_temp$tmax[daily_temp$station == "Kisumu" & daily_temp$date == as.Date("1995-01-01")] <- NA

# Big tmax - tmin values
maxmindiff <- daily_temp %>% filter(tmax - tmin > 30)
if(nrow(maxmindiff) > 0) View(maxmindiff)
# Possible incorrect values
View(daily_temp %>% filter(station == "Livingstone" & month == 9 & year == 1981))

# monthly boxplots
nest_station <- daily_temp %>%
  group_by(country, station) %>%
  nest()
f <- function(data, name) {
  ggplot(data, aes(x = factor(month), y = tmax - tmin)) +
    geom_hline(yintercept = 0, colour = "blue") +
    geom_boxplot() +
    ggtitle(name)
}

nest_station <- nest_station %>% mutate(month_boxs_diff = purrr::map2(.x = data, .y = paste(country, station, sep = "."),
                                                                      .f = f))
nest_station$month_boxs_diff

# tmax high, jump and diff too high, make tmax missing
daily_temp$tmax[daily_temp$station == "Sadore" & daily_temp$date == as.Date("2006-07-08")] <- NA

daily_temp <- daily_temp %>%
  mutate(tmax_diff = tmax - dplyr::lag(tmax),
         tmax_diff2 = dplyr::lead(tmax) - tmax,
         tmin_diff = tmin - dplyr::lag(tmin),
         tmin_diff2 = dplyr::lead(tmin) - tmin)

# Large difference over 2 days
bigdiff <- daily_temp %>% 
  filter(abs(tmin_diff) > 10 | abs(tmin_diff2) > 10) %>%
  dplyr::select(country, station, date, tmax, tmax_diff, tmin, tmin_diff)
if(nrow(bigdiff) > 0) View(bigdiff)

# Possibly incorrect but not clear
View(daily_temp %>% filter(station == "Sadore" & month == 3 & year == 1993))

# tmin high and big diff, make tmin missing
View(daily_temp %>% filter(station == "Sadore" & month == 12 & year == 1995))
daily_temp$tmin[daily_temp$station == "Sadore" & daily_temp$date == as.Date("1995-12-03")] <- NA

# Possibly too high tmin but tmax also high so not clear
View(daily_temp %>% filter(station == "Sadore" & month == 7 & year == 2003))

# tmin too low and big diffs, should be + 10
View(daily_temp %>% filter(station == "Sadore" & month == 3 & year == 2008))
daily_temp$tmin[daily_temp$station == "Sadore" & daily_temp$date == as.Date("2008-03-24")] <- 25.6

# Possibly too high tmin but not clear
View(daily_temp %>% filter(station == "Livingstone" & month == 6 & year == 1984))
# Too high tmin and big diffs, should be - 10
View(daily_temp %>% filter(station == "Livingstone" & month == 5 & year == 1995))
daily_temp$tmin[daily_temp$station == "Livingstone" & daily_temp$date == as.Date("1995-05-05")] <- 15.4

# Too low tmin and big diffs, should be +ve values
View(daily_temp %>% filter(station == "Livingstone" & month == 6 & year == 1995))
daily_temp$tmin[daily_temp$station == "Livingstone" & daily_temp$date == as.Date("1995-06-17")] <- 5.5
daily_temp$tmin[daily_temp$station == "Livingstone" & daily_temp$date == as.Date("1995-06-18")] <- 5.9
daily_temp$tmin[daily_temp$station == "Livingstone" & daily_temp$date == as.Date("1995-06-19")] <- 5.5

# Possibly reasonable diffs
View(daily_temp %>% filter(station == "Livingstone" & month == 5 & year == 2005))

# Possibly too high tmin, one big diff but not clear
View(daily_temp %>% filter(station == "Livingstone" & month %in% c(6, 7) & year == 2005))

# Too low tmin, big diffs, should be + 10
View(daily_temp %>% filter(station == "Mpika" & month %in% c(5) & year == 1988))
daily_temp$tmin[daily_temp$station == "Mpika" & daily_temp$date == as.Date("1988-05-19")] <- 12.4

# Too low tmin, big diffs, make tmin missing
View(daily_temp %>% filter(station == "Mpika" & month %in% c(7) & year == 1992))
daily_temp$tmin[daily_temp$station == "Mpika" & daily_temp$date == as.Date("1992-07-14")] <- NA

View(daily_temp %>% filter(station == "Mpika" & month %in% c(9) & year == 1992))

# Consecutive values check
consec_check <- daily_temp %>% 
  group_by(country, station) %>%
  mutate(same_tmax = rep(rle(as.numeric(tmax))$lengths, rle(as.numeric(tmax))$lengths),
         same_tmin = rep(rle(as.numeric(tmin))$lengths, rle(as.numeric(tmin))$lengths)) %>%
  filter(same_tmin >= 5) %>%
  dplyr::select(date, tmin, same_tmin, tmax, same_tmax)
if(nrow(consec_check) > 0) View(consec_check)

# Some suspect consecutive values: 12, 7, some 6 and 5, not clear how to resolve
View(daily_temp %>% filter(station == "Sadore" & month %in% c(7, 8) & year == 2007))

# homogeneity and trend checks
nest_station <- daily_temp %>%
  group_by(country, station) %>%
  nest()
f <- function(data, name, var) {
  ggplot(data, aes(x = factor(year), y = {{ var }})) +
    geom_boxplot(varwidth = TRUE) +
    ggtitle(name)
}

nest_station <- nest_station %>% mutate(trend_tmax = purrr::map2(.x = data, .y = paste(country, station, sep = "."),
                                                                 .f = f, var = tmax),
                                        trend_tmin = purrr::map2(.x = data, .y = paste(country, station, sep = "."),
                                                                 .f = f, var = tmin))
nest_station$trend_tmin
nest_station$trend_tmax

daily_temp <- daily_temp %>%
  dplyr::select(country, station, date, year, month, day, tmax, tmin)

saveRDS(daily_temp, here("data", "temperature_1979_qc.RDS"))
write.csv(daily_temp, here("data", "temperature_1979_qc.csv"), row.names = FALSE)