#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GREEEN prairie project
# Show figure ####
# RLQ ~ herbicide * seeding time (Lux Arbor)
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
library(patchwork)

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
    legend.position = "bottom",
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
    treatment_id %in% c("1", "2", "3", "4")
  ) %>%
  mutate(
    treatment = str_c(seeding_time, herbicide, seeded_pool, sep = "_"),
    treatment = factor(treatment)
  ) %>%
  #filter(treatment != "unseeded_0_0") %>%
  select(
    id_plot_year, year, treatment, water_cap, cover_non_seeded
  )

### * Model ####
model <- read_csv(
  here("outputs", "models", "model_rlq_timing_herbicide_lux_arbor.csv"),
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
    )

se <- function(z) {
  qt(0.025, df = length(z) - 1, lower.tail = FALSE) * sd(z) / sqrt(length(z))
  } # function to calculate std.err

centroid <- data %>%
  filter(type == "sites") %>%
  group_by(year, treatment) %>%
  summarize(
    mean1 = mean(axis1), mean2 = mean(axis2), sd1 = sd(axis1), sd2 = sd(axis2),
    se1 = se(axis1), se2 = se(axis2)
    ) %>%
  mutate(
    treatment = str_replace(treatment, "treat.fall_0_33", "Autumn"),
    treatment = str_replace(treatment, "treat.spring_0_33", "Spring"),
    treatment = str_replace(treatment, "treat.spring_1_33", "Spring + Herbicide"),
  ) %>%
  ungroup()



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



graph_a <- ggplot(
  data = centroid, aes(x = mean1, y = mean2, color = treatment, label = year)
) +
  geom_point(shape = 16, alpha = 1, size = 2) +
  geom_errorbar(
    aes(ymin = mean2 - se2, ymax = mean2 + se2, color = treatment),
    width = 0, alpha = 0.5
    ) +
  geom_errorbarh(
    aes(xmin = mean1 - se1, xmax = mean1 + se1, color = treatment),
    height = 0, alpha = 0.5
    ) +
  geom_path(size = 0.6) +
  geom_text_repel(size = 3, color = "black") +
  annotate(
    "label", x = -.6, y = 1, color = "#FFA500", size = 3, label = "Spring"
  ) +
  annotate(
    "label", x = 1.7, y = 1.7, color = "#FFA570", size = 3,
    label = "Spring +\nHerbicide"
  ) +
  annotate(
    "label", x = -.1, y = -1.3, color = "#440154", size = 3, label = "Autumn"
  ) +
  coord_fixed(xlim = c(-2.4, 2.5), ylim = c(-2.7, 2)) +
  scale_color_manual(
    breaks = c("spring_0_33", "spring_1_33", "fall_0_33"),
    labels = c("Spring", "Spring + Herbicide", "Autumn"),
    values = c("#FFA500", "#FFA570", "#440154")
  ) +
  labs(
    x = "Principal Axis 1", color = "Seeding", y = "Principal Axis 2",
    title = "Lux Arbor"
  ) +
  theme_mb() +
  theme(legend.position = "none");graph_a

### Save ###
ggsave(
  here("outputs", "figures", "figure_3a_300dpi_8x8cm_centroid.tiff"),
  dpi = 300, width = 8, height = 8, units = "cm"
)
