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

sf_af <- ne_countries(returnclass = "sf", continent = "africa")

locs <- readRDS(here("data", "station", "cleaned", "temperature", "temp_locations.RDS"))

width <- 0.25

kenya_sf <- read_sf("C:/Users/Danny/Downloads/kenyan-counties/County.shp")
vict_sf <- read_sf("C:/Users/Danny/Downloads/dataverse_files/LakeVictoriaShoreline.shp")
dem <- raster::raster("C:/Users/Danny/Downloads/Kenya_SRTM30meters/Kenya_SRTM30meters.tif")
# Kisumu pixel
kis_dem <- crop(dem, extent(34.75 - width/2, 34.75 + width/2, 0 - width/2, 0 + width/2))
# whole area
# kis_dem <- crop(dem, extent(34, 35, -0.5, 0.5))
kis_df <- as.data.frame(kis_dem, xy = TRUE)
names(kis_df) <- c("x", "y", "elevation")

g <- ggplot() +
  geom_raster(data = kis_df, aes(x = x, y = y, fill = elevation), interpolate = TRUE) +
  geom_sf(data = sf_af, fill = NA) +
  geom_sf(data = vict_sf, fill = "lightblue", colour = "black") +
  geom_point(data = locs, aes(x = req_longitude, y = req_latitude), colour = "white") +
  geom_rect(data = locs, aes(xmin = Var1 - width/2, xmax = Var1 + width/2,
                             ymin = Var2 - width/2, ymax = Var2 + width/2), 
            colour = "red", fill = NA, size = 1.5) +
  geom_text(data = locs, aes(x = Var1, y = Var2, label = "ERA5", fontface = "italic"), colour = "white", size = 7) +
  annotate(geom = "text", x = 34.73, y = -0.05, 
           label = "Kisumu", size = 7, colour = "white") +
  annotate(geom = "text", x = 34.60, y = 0.03, 
           label = "Maseno", size = 7, colour = "white") +
  annotate(geom = "point", x = 34.60, y = 0, colour = "white") +
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
