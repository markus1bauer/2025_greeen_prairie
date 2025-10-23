#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GREEEN prairie project
# Show figure ####
# RLQ ~ pool * seeding timing/herbicide (SW Station)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2025-10-23



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
  arrange(id_plot_year) %>%
  filter(
    site == "SW Station",
    richness_type == "seeded_richness",
    treatment_id %in% c("1", "2", "3", "4")
  ) %>%
  mutate(
    treatment = str_c(seeding_time, herbicide, seeded_pool, sep = "_"),
    treatment = factor(treatment)
  ) %>%
  filter(treatment != "unseeded_0_0") %>%
  select(
    id_plot_year, year, treatment, water_cap, cover_non_seeded,
    cover_seeded_grass
  )

### * Model ####
model <- read_csv(
  here("outputs", "models", "model_rlq_pool_sw_station.csv"),
  col_names = TRUE, na = c("na", "NA", ""), col_types = cols(
    .default = "?",
    type = "f"
  )
)

data <- model %>%
  left_join(sites %>% rename(id = id_plot_year), by = "id") %>%
  mutate(
    facet = if_else(
      type == "sites" | type == "species", "Species and sites", if_else(
        type == "environment" | type == "traits", "Traits and environment",
        "warning"
      )
    ),
    id = str_replace(id, "treat.unseeded_0_0", "Unseeded"),
    id = str_replace(id, "treat.fall_0_33", "Fall"),
    id = str_replace(id, "treat.spring_0_33", "Spring"),
    id = str_replace(id, "treat.spring_1_33", "Spring + Herb"),
    id = str_replace(id, "sla", "SLA"),
    id = str_replace(id, "height", "Height"),
    id = str_replace(id, "seedmass", "Seed mass"),
    id = str_replace(id, "cover_seeded_grass", "Cover seeded grass"),
    id = str_replace(id, "cover_non_seeded", "Cover non-seeded sp."),
    id = str_replace(id, "water_cap", "Water caption"),
    type = if_else(
      id %in% c("Fall", "Spring", "Spring + Herb"), "environment_factor", type
    )
  ) %>%
  filter(type != "traits", id != "year")



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



graph_f <- ggplot() +
  geom_segment(
    data = data %>% filter(type %in% c("traits", "environment")),
    aes(x = 0, y = 0, xend = axis1, yend = axis2, color = type),
    arrow = arrow(length = unit(.2, "cm"), type = "closed")
  ) +
  geom_label_repel(
    data = data %>% filter(type %in% c("traits", "environment")),
    aes(x = axis1, y = axis2, label = id, color = type),
    fill = alpha("white", 0), label.size = NA, max.overlaps = 50,
    size = 3
  ) +
  annotate(
    "text", y = .03, x = .22, size = 3,
    label = "environment: p = .001\n traits: p = .450"
  ) +
  coord_fixed() +
  scale_color_manual(
    breaks = c("traits", "environment", "environment_factor"),
    values = c("red", "blue", "blue")
  ) +
  labs(
    x = "Principal Axis 1", color = "", y = "Principal Axis 2",
    title = "SW Station"
  ) +
  theme_mb() +
  theme(legend.position = "none");graph_f

### Save ###
ggsave(
  here("outputs", "figures", "figure_4f_300dpi_8x8cm.tiff"),
  dpi = 300, width = 8, height = 8, units = "cm"
)
