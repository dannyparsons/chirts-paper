library(here)
library(ggplot2)
library(lubridate)
library(reshape2)
library(viridis)
library(RColorBrewer)
library(tidyr)
library(rnaturalearthdata)
library(rnaturalearth)
library(ggrepel)
library(sp)
library(tibble)
library(purrr)
library(dplyr)
library(sf)
library(ggspatial)
library(raster)

locs_era5 <- readRDS(here("data", "temp_locations_era5.RDS"))
locs_era5$width <- 0.25
locs_era5land <- readRDS(here("data", "temp_locations_era5land.RDS"))
locs_era5land$width <- 0.1
locs_chirts <- readRDS(here("data", "temp_locations_chirts.RDS"))
locs_chirts$width <- 0.05
locs <- bind_rows(era5 = locs_era5, era5_land = locs_era5land, chirts = locs_chirts, 
                  .id = "product")
locs$Var1 <- as.vector(locs$Var1)
locs$Var2 <- as.vector(locs$Var2)

names(locs)[2:3] <- c("lon", "lat")

locs_kisumu <- locs %>% filter(station == "Kisumu")

kis_lon <- first(locs_kisumu$req_longitude)
kis_lat <- first(locs_kisumu$req_latitude)

width <- 0.25

sf_af <- ne_countries(returnclass = "sf", continent = "africa")
kenya_sf <- ne_countries(returnclass = "sf", country = "kenya")
kenya_sf2 <- read_sf("C:/Users/Danny/Downloads/kenyan-counties/County.shp")

# Lake Victoria coastline from https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/PWFW26
# Cite: Hamilton, Stuart, 2016, "Shoreline, Lake Victoria, vector polygon, ~2015", https://doi.org/10.7910/DVN/PWFW26, Harvard Dataverse, V3
# Terms: CC0 - "Public Domain Dedication"
vict_sf <- read_sf(here("data", "Lake Victoria", "LakeVictoriaShoreline.shp"))

# Digital Elevation Model for Kenya from http://geoportal.rcmrd.org/layers/servir%3Akenya_srtm30meters
# Terms: Public Domain (PD)
dem <- raster::raster(here("data", "Kenya_SRTM30m", "Kenya_SRTM30meters.tif"))

# Kisumu pixel
kisuma_ele <- crop(dem, extent(34.4, 35, -0.3, 0.3))

# whole area
# kis_dem <- crop(dem, extent(34, 35, -0.5, 0.5))
kis_df <- as.data.frame(kisuma_ele, xy = TRUE)
names(kis_df) <- c("x", "y", "elevation")

for (i in 1: nrow(locs_kisumu)) {
  era5_lon <- locs_kisumu$lon[i]
  era5_lat <- locs_kisumu$lat[i]
  width <- locs_kisumu$width[i]
  kis_era5 <- crop(dem, extent(era5_lon - width/2, era5_lon + width/2, 
                               era5_lat - width/2, era5_lat + width/2))
  kis_era5 <- as.data.frame(kis_era5, xy = TRUE)
  print(paste(locs_kisumu$product[i], "Mean elevation: ", round(mean(kis_era5$Kenya_SRTM30meters), 0)))
}

ggplot() +
  geom_sf(data = sf_af) +
  geom_raster(data = kis_df, aes(x = x, y = y, fill = elevation), interpolate = TRUE) +
  geom_sf(data = vict_sf, fill = "lightblue", colour = "black") +
  geom_point(data = locs_kisumu, aes(x = req_longitude, y = req_latitude)) +
  geom_rect(data = locs_kisumu, aes(xmin = lon - width/2, xmax = lon + width/2,
                             ymin = lat - width/2, ymax = lat + width/2, 
                             colour = product),
            fill = NA, size = 1.5) +
  coord_sf(xlim = c(34.4, 35), ylim = c(-0.3, 0.3), expand = FALSE) +
  scale_x_continuous(breaks = seq(34, 35, 0.1)) +
  scale_y_continuous(breaks = seq(-0.5, 0.5, 0.1)) +
  annotation_scale(location = "bl", width_hint = 0.5) + 
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in"), 
                         style = north_arrow_fancy_orienteering) +
  scale_fill_viridis() +
  theme_bw() +
  theme(panel.grid.major = element_line(color = gray(0.7), linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"))


g <- ggplot() +
  geom_sf(data = sf_af) +
  geom_raster(data = kis_df, aes(x = x, y = y, fill = elevation), interpolate = TRUE) +
  geom_sf(data = vict_sf, fill = "lightblue", colour = "black") +
  #geom_point(data = locs, aes(x = req_longitude, y = req_latitude), colour = "white") +
  #geom_rect(data = locs, aes(xmin = lon - width/2, xmax = lon + width/2,
  #                           ymin = lat - width/2, ymax = lat + width/2), 
  #          colour = "red", fill = NA, size = 1.5) +
  #geom_text(data = locs, aes(x = lon, y = lat, label = "ERA5", fontface = "italic"), colour = "white", size = 7) +
  coord_sf(xlim = c(34, 35), ylim = c(-0.5, 0.5), expand = FALSE) +
  scale_x_continuous(breaks = seq(34, 35, 0.1)) +
  scale_y_continuous(breaks = seq(-0.5, 0.5, 0.1)) +
  annotation_scale(location = "bl", width_hint = 0.5) + 
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in"), 
                         style = north_arrow_fancy_orienteering) +
  scale_fill_viridis() +
  theme_bw() +
  theme(panel.grid.major = element_line(color = gray(0.7), linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"))
g

# elevation investigation
kis_el <- mean((kis_df %>% filter(abs(x - 34.728) < 0.001 & abs(y - (-0.083) < 0.001)))$elevation)

ggplot(kis_df, aes(x = elevation)) + 
  geom_density() +
  geom_vline(colour = "red", xintercept = mean(kis_df$elevation)) +
  geom_vline(colour = "blue", xintercept = median(kis_df$elevation)) +
  geom_vline(colour = "brown", xintercept = kis_el)
  
saveRDS(g, here("results", "temperature", "kisumu_map.RDS"))
ggsave(here("results", "temperature", "kisumu_map.png"), width = 12, height = 12)

# stars
library(stars)
st_kis <- read_stars("C:/Users/Danny/Downloads/Kenya_SRTM30meters/Kenya_SRTM30meters.tif")
ggplot() + geom_stars(data = st_kis, downsample = c(10, 10, 1))
