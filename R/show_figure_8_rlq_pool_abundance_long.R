#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GREEEN prairie project
# Show figure ####
# Seeded species richness ~ pool size * seeding time
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2025-08-21



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
rm(list = setdiff(ls(), c("graph_a", "graph_b", "graph_c", "graph_d", "m")))

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
    site == "Lux Arbor",
    richness_type == "seeded_richness",
    !(treatment_id %in% c("2", "4"))
  ) %>%
  select(
    id_plot_year, year, seeded_pool, water_cap, seeding_time, cover_non_seeded,
    cover_seeded_grass
  )

### * Model ####
model <- read_csv(
  here("outputs", "models", "model_rlq_pool_abundance_long.csv"),
  col_names = TRUE, na = c("na", "NA", ""), col_types = cols(
    .default = "?",
    type = "f"
  )
) #%>%
  #mutate(id = str_replace(id, " ", "\n"))

data <- model %>%
  left_join(sites %>% rename(id = id_plot_year), by = "id") %>%
  mutate(
    facet = if_else(
      type == "sites" | type == "species", "Species and sites", if_else(
        type == "environment" | type == "traits", "Traits and environment",
        "warning"
      )
      )
    )



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



graph_a <- ggplot() +
  geom_point(
    data = data %>% filter(type == "sites"),
    aes(x = axis1, y = axis2, color = seeding_time),
    shape = 16, alpha = .7
  ) +
  geom_point(
    data = data %>% filter(type == "species"),
    aes(x = axis1, y = axis2), shape = 4
  ) +
  geom_label_repel(
    data = data %>% filter(type == "species"),
    aes(x = axis1, y = axis2, label = id),
    xlim = c(-3.3, Inf), ylim = c(NA, Inf),
    fill = alpha("white", .5), label.size = NA, max.overlaps = 50,
    size = 3
  ) +
  geom_segment(
    data = data %>% filter(type == "environment"),
    aes(x = 0, y = 0, xend = axis1, yend = axis2),
    arrow = arrow(length = unit(.2, "cm"), type = "closed"),
    color = "blue"
  ) +
  geom_label_repel(
    data = data %>% filter(type == "environment"),
    aes(x = axis1, y = axis2, label = id),
    fill = alpha("white", .5), label.size = NA, max.overlaps = 50,
    size = 3, color = "blue"
  ) +
  geom_segment(
    data = data %>% filter(type == "traits"),
    aes(x = 0, y = 0, xend = axis1, yend = axis2),
    arrow = arrow(length = unit(.2, "cm"), type = "closed"),
    color = "red"
  ) +
  geom_label_repel(
    data = data %>% filter(type == "traits"),
    aes(x = axis1, y = axis2, label = id),
    fill = alpha("white", .5), label.size = NA, max.overlaps = 50,
    size = 3, color = "red"
  ) +
  facet_wrap(~facet, nrow = 2) +
  coord_fixed(clip = "off") +
  scale_color_manual(
    breaks = c("fall", "spring"),
    labels = c("Fall", "Spring"),
    values = c("#440154", "#FFA500")
  ) +
  labs(
    x = "Axis 1", color = "Seeding", y = "Axis 2"
  ) +
  theme_mb();graph_a

### Save ###
ggsave(
  here("outputs", "figures", "figure_8_300dpi_16x18cm.tiff"),
  dpi = 300, width = 16, height = 18, units = "cm"
  )
