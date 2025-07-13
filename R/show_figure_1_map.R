#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GREEEN Prairie Project
# Show figure map ####
# Map of the study sites
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2025-07-13



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ###############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### Packages ###
library(here)
library(tidyverse)
library(sf)

### Start ###
rm(list = ls())

### Functions ###
theme_mb <- function() {
  theme(
    panel.background = element_rect(fill = NA),
    panel.grid = element_line(colour = NA),
    text = element_text(size = 10, color = "black"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    legend.key = element_rect(fill = "white"),
    legend.margin = margin(0, 0, 0, 0, "cm"),
    plot.margin = margin(.5, 0, 0, 0, "cm")
  )
}

### Load data ###

sites <- data.frame(
  longitude = c(-85.44965363680959),
  latitude = c(42.48875440841494)
) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

ecoregions <- st_read(here("data", "raw", "ecoregions2017.shp")) %>%
  st_transform(crs = 4326) %>%
  filter(
    ECO_NAME == "Southern Great Lakes forests" |
      ECO_NAME == "Western Great Lakes forests" |
      ECO_NAME == "Upper Midwest US forest-savanna transition" |
      ECO_ID == 328 |
      ECO_ID == 329 |
      ECO_ID == 331 |
      ECO_ID == 333 |
      ECO_ID == 334 |
      ECO_ID == 336 |
      ECO_ID == 338 |
      ECO_ID == 339 |
      ECO_ID == 347 |
      ECO_ID == 370 |
      ECO_ID == 377 |
      ECO_ID == 387 |
      ECO_ID == 388 |
      ECO_ID == 393 |
      ECO_ID == 399
    )

crop <- st_crop(ecoregions, xmin = -95, xmax = -70, ymin = 38.5, ymax = 48)

elevation <- elevatr::get_elev_raster(
  locations = data.frame(x = c(-95, -70), y = c(39, 48)),
  prj = "+proj=longlat +datum=WGS84 +no_defs",
  clip = "bbox", z = 5
) %>%
  terra::rast() %>%
  raster::mask(ecoregions)

rivers <- rnaturalearth::ne_download(
  scale = 10, type = 'rivers_lake_centerlines', category = 'physical'
) %>%
  st_crop(xmin = -95, xmax = -70, ymin = 39, ymax = 47)

gradient_1 <- tidyterra::hypso.colors2(10, "dem_poster")



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot #######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## 1 Map with ecoregions #######################################################


graph_sites <- ggplot() +
  # geom_sf(
  #   data = ecoregions, colour = "black", fill = "transparent", size = 1,
  #   linetype = "dashed"
  # ) +
  # geom_sf(data = sites, colour = "black", size = 1) +
  # annotate(
  #   geom = "label", x = c(9.9, 12.85, 11), y = c(52.8, 51.55, 48.7),
  #   label = c("ID 664", "ID 654", "ID 686"), fill = "transparent", size = 3.5
  # ) +
  # annotate(
  #   geom = "text", x = c(8, 13, 13.8, 10), y = c(53.1, 53.85, 52.2, 50),
  #   lineheight = .8, size = 3.5,
  #   label = c(
  #     "European Atlantic\nmixed forests",
  #     "Baltic\nmixed forests",
  #     "Central European\nmixed forests",
  #     "Western European\nbroadleaf forests"
  #     )
  # ) +
  ggspatial::annotation_north_arrow(
    which_north = "true",
    style = ggspatial::north_arrow_fancy_orienteering(),
    height = unit(2, "cm"),
    width = unit(2, "cm"),
    pad_y = unit(.5, "cm"),
    pad_x = unit(0.1, "cm")
  ) +
  ggspatial::annotation_scale(
    pad_y = unit(.7, "cm"),
    pad_x = unit(2, "cm")
  ) +
  theme_mb();graph_sites

### Save -----------------------------------------------------------------------

# ggsave(
#   "figure_1_map_ecoregions_300dpi_12x15cm.tiff",
#   dpi = 300, width = 12, height = 15, units = "cm",
#   path = here("outputs", "figures")
# )



## 2 Map with elevation ########################################################


graph_sites <- ggplot() +
  tidyterra::geom_spatraster(data = elevation) +
  geom_sf(
    data = crop, colour = "black", fill = "transparent", size = 1,
    linetype = "solid"
  ) +
  geom_sf(data = rivers, color = "blue", linewidth = 0.2) +
  annotate(
    geom = "text", label = c("Mississippi", "Grand River"),
    x = c(-92, -84.6), y = c(43.4, 43.3), angle = c(295, 350),
    color = "blue", size = 2.5
  ) +
  geom_sf(data = sites, colour = "black", size = 1) +
  annotate(
    geom = "label", x = c(-83.5), y = c(42.5),
    label = c("Lux Arbor"), fill = "transparent", size = 3,
  ) +
  # annotate(
  #   geom = "text", x = c(7.6, 13, 13.4, 10.4), y = c(53, 53.82, 52.2, 49.7),
  #   lineheight = .8, size = 3.5,
  #   label = c(
  #     "European Atlantic\nmixed forests",
  #     "Baltic\nmixed forests",
  #     "Central European\nmixed forests",
  #     "Western European\nbroadleaf forests"
  #   )
  # ) +
  ggspatial::annotation_north_arrow(
    which_north = "true",
    style = ggspatial::north_arrow_fancy_orienteering(),
    height = unit(1.5, "cm"),
    width = unit(1.5, "cm"),
    pad_y = unit(.5, "cm"),
    pad_x = unit(.3, "cm")
  ) +
  ggspatial::annotation_scale(
    pad_y = unit(.8, "cm"),
    pad_x = unit(2.2, "cm")
  ) +
  scale_fill_gradientn(
    colours = gradient_1, na.value = NA, name = "Elevation [m]"
    ) +
  theme_mb();graph_sites


### Save -----------------------------------------------------------------------

ggsave(
  "figure_1_map_elevation_300dpi_15x9cm.tiff",
  dpi = 300, width = 15, height = 9, units = "cm",
  path = here("outputs", "figures")
)
