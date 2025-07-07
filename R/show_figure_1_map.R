#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GRASSWORKS Project
# Show figure map ####
# Map of the study sites
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2025-05-20



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

sites <- st_read(
  here("data", "raw", "data_processed_sites_epsg4326.gpkg")
  ) %>%
  filter(
    eco.id %in% c(664, 654, 686) & !(eco.id == 686 & region == "centre")
    )

ecoregions <- st_read(here("data", "raw", "ecoregions2017.shp")) %>%
  st_transform(crs = 4326) %>%
  filter(
    ECO_NAME == "Western European broadleaf forests" |
      ECO_NAME == "Central European mixed forests" |
      ECO_NAME == "European Atlantic mixed forests" |
      ECO_NAME == "Baltic mixed forests" |
      ECO_NAME == "Alps conifer and mixed forests"
  )

germany <- geodata::gadm(
  country = "DEU", level = 0, version = "latest", resolution = 2, path = here()
) %>%
  st_as_sf() %>%
  st_set_crs(4326)

elevation <- elevatr::get_elev_raster(
  locations = data.frame(x = c(5.5, 15.5), y = c(47, 55.5)),
  prj = "+proj=longlat +datum=WGS84 +no_defs",
  clip = "bbox", z = 5
) %>%
  terra::rast()

rivers <- rnaturalearth::ne_download(
  scale = 50, type = 'rivers_lake_centerlines', category = 'physical'
) %>%
  st_crop(xmin = 5.5, xmax = 15.5, ymin = 47, ymax = 55.4)



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Create variables ###########################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



clip <- st_intersection(ecoregions, germany)
crop <- st_crop(ecoregions, xmin = 5.5, xmax = 15.5, ymin = 47, ymax = 55.4)
mask <- mask(elevation, crop)
gradient_1 <- tidyterra::hypso.colors2(10, "dem_poster")



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# C Plot #######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## 1 Map with ecoregions #######################################################


graph_sites <- ggplot() +
  geom_sf(
    data = clip, colour = "black", fill = "transparent", size = 1,
    linetype = "dashed"
  ) +
  geom_sf(data = germany, colour = "black", fill = "transparent", size = 1) +
  geom_sf(data = sites, colour = "black", size = 1) +
  annotate(
    geom = "label", x = c(9.9, 12.85, 11), y = c(52.8, 51.55, 48.7),
    label = c("ID 664", "ID 654", "ID 686"), fill = "transparent", size = 3.5
  ) +
  annotate(
    geom = "text", x = c(8, 13, 13.8, 10), y = c(53.1, 53.85, 52.2, 50),
    lineheight = .8, size = 3.5,
    label = c(
      "European Atlantic\nmixed forests",
      "Baltic\nmixed forests",
      "Central European\nmixed forests",
      "Western European\nbroadleaf forests"
      )
  ) +
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
  tidyterra::geom_spatraster(data = mask) +
  geom_sf(
    data = crop, colour = "black", fill = "transparent", size = 1,
    linetype = "solid"
    ) +
  geom_sf(data = rivers, color = "blue", linewidth = 0.2) +
  annotate(
    geom = "text", label = c("Danube", "Elbe"),
    x = c(13.5, 11.7), y = c(48.95, 53.25),
    angle = 333, color = "blue", size = 3.5
  ) +
  geom_sf(data = sites, colour = "black", size = 1) +
  annotate(
    geom = "label", x = c(9.85, 14, 11), y = c(52.85, 51.7, 49.2),
    label = c("ID 664", "ID 654", "ID 686"), fill = "transparent", size = 3.5,
  ) +
  annotate(
    geom = "text", x = c(7.6, 13, 13.4, 10.4), y = c(53, 53.82, 52.2, 49.7),
    lineheight = .8, size = 3.5,
    label = c(
      "European Atlantic\nmixed forests",
      "Baltic\nmixed forests",
      "Central European\nmixed forests",
      "Western European\nbroadleaf forests"
    )
  ) +
  ggspatial::annotation_north_arrow(
    which_north = "true",
    style = ggspatial::north_arrow_fancy_orienteering(),
    height = unit(2, "cm"),
    width = unit(2, "cm"),
    pad_y = unit(.6, "cm"),
    pad_x = unit(0.2, "cm")
  ) +
  ggspatial::annotation_scale(
    pad_y = unit(.9, "cm"),
    pad_x = unit(2.2, "cm")
  ) +
  scale_fill_gradientn(
    colours = gradient_1, na.value = NA, name = "Elevation [m]"
    ) +
  theme_mb();graph_sites


### Save -----------------------------------------------------------------------

ggsave(
  "figure_1_map_elevation_300dpi_15x15cm.tiff",
  dpi = 300, width = 15, height = 15, units = "cm",
  path = here("outputs", "figures")
)
