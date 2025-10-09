#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GREEEN prairie project
# Show figure ####
# RLQ ~ herbicide * seeding time (NW Station)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2025-09-09



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ###############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### Packages ###
library(here)
library(tidyverse)
library(ade4)
library(ggplot2)
library(ggrepel)

### Start ###
rm(list = setdiff(ls(), c("graph_a", "graph_b", "graph_c", "graph_d",
                          "graph_e", "graph_f")))

### Functions ###
theme_mb <- function() {
  theme(
    panel.background = element_rect(fill = "white"),
    text = element_text(size = 9, color = "black"),
    strip.text = element_text(size = 10),
    axis.text = element_text(angle = 0, hjust = 0.5, size = 9,
                             color = "black"),
    axis.title = element_text(angle = 0, hjust = 0.5, size = 9,
                              color = "black"),
    axis.line = element_line(),
    legend.key = element_rect(fill = "white"),
    legend.position = "right",
    legend.text = element_text(size = 9),
    legend.margin = margin(0, 0, 0, 0, "cm"),
    plot.margin = margin(0, 0, 0, 0, "cm"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
}

### Load data ####

sites <- read_csv(
  here("data", "processed", "data_processed_sites.csv"),
  col_names = TRUE, na = c("na", "NA", ""), col_types =
    cols(
      .default = "?",
      seeding_time = "f"
    )) %>% 
  filter(
    site == "NW Station",
    richness_type == "seeded_richness",
    treatment_id %in% c("1", "2", "3", "4")
  ) %>%
  mutate(
    treatment = str_c(seeding_time, herbicide, seeded_pool, sep = "_"),
    treatment = factor(treatment)
    ) %>%
  select(
    id_plot_year, year, treatment, water_cap, cover_non_seeded, site
  )

### * Model ####
model <- read_csv(
  here("outputs", "models", "model_rlq_timing_herbicide_nw_station.csv"),
  col_names = TRUE, na = c("na", "NA", ""), col_types = cols(
    .default = "?",
    type = "f"
  )
)

data <- model %>%
  left_join(sites %>% rename(id = id_plot_year) %>% select(-site), by = "id") %>%
  mutate(
    facet = if_else(
      type == "sites" | type == "species", "Species and sites", if_else(
        type == "environment" | type == "traits", "Traits and environment",
        "warning"
      )
      ),
    id = str_replace(id, "treat.unseeded_0_0", "Unseeded"),
    id = str_replace(id, "treat.fall_0_33", "Autumn"),
    id = str_replace(id, "treat.spring_0_33", "Spring"),
    id = str_replace(id, "treat.spring_1_33", "Spring + Herbicide"),
    id = str_replace(id, "sla", "SLA"),
    id = str_replace(id, "height", "Height"),
    id = str_replace(id, "seedmass", "Seed mass"),
    id = str_replace(id, "year", "Year"),
    id = str_replace(id, "cover_seeded_grass", "Cover seeded grass"),
    id = str_replace(id, "cover_non_seeded", "Cover non-seeded sp."),
    id = str_replace(id, "water_cap", "Water caption"),
    type = if_else(
      id %in% c("Autumn", "Spring", "Spring + Herbicide"), "environment_factor", type
      )
    ) %>%
  filter(type != "traits")



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



graph_c <- ggplot() +
  geom_point(
    data = data %>% filter(type == "sites"),
    aes(x = axis1, y = axis2, color = treatment),
    shape = 16, alpha = .8
  ) +
  geom_label(
    data = data %>% filter(type == "environment_factor"),
    aes(x = axis1, y = axis2, label = id),
    fill = alpha("white", .8),
    size = 3
  ) +
  # geom_point(
  #   data = data %>% filter(type == "species"),
  #   aes(x = axis1, y = axis2), shape = 4
  # ) +
  # geom_label_repel(
  #   data = data %>% filter(type == "species"),
  #   aes(x = axis1, y = axis2, label = id),
  #   xlim = c(NA, Inf), ylim = c(NA, Inf),
  #   fill = alpha("white", .5), label.size = NA, max.overlaps = 50,
  #   size = 3
  # ) +
  coord_fixed(xlim = c(-2.5, 2.5), ylim = c(-2.2, 2.2)) +
  scale_color_manual(
    breaks = c("unseeded", "fall_0_33", "spring_0_33", "spring_1_33"),
    labels = c("Unseeded", "Autumn", "Spring", "Spring + Herbicide"),
    values = c("#21918c", "#440154", "#FFA500", "#FFA570")
  ) +
  labs(
    x = "Principal Axis 1", color = "Seeding", y = "Principal Axis 2",
    title = "NW Station"
  ) +
  theme_mb() +
  theme(legend.position = "none");graph_c

### Save ###
ggsave(
  here("outputs", "figures", "figure_3c_300dpi_8x8cm.tiff"),
  dpi = 300, width = 8, height = 8, units = "cm"
)
