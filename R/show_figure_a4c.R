#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GREEEN prairie project
# Site SW Station
# Show figure ####
# Establishment rate ~ pool size * seeding time
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2026-02-12



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ###############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### Packages ###
library(here)
library(tidyverse)
library(ggeffects)
library(ggbeeswarm)

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

#### Load data ###
data <- read_csv(
  here("data", "processed", "data_processed_sites.csv"),
  col_names = TRUE, na = c("na", "NA", ""), col_types = cols(
    .default = "?",
    id_plot_year = "f",
    id_plot = "f",
    site = col_factor(
      levels = c("NW Station", "Lux Arbor", "SW Station"), ordered = FALSE
    ),
    year = "f",
    seeding_time = col_factor(
      levels = c("unseeded", "fall", "spring"), ordered = FALSE
    ),
    herbicide = col_factor(levels = c("0", "1"), ordered = FALSE),
    seeded_pool = col_factor(
      levels = c("0", "6", "12", "18", "33"), ordered = TRUE
    ),
    treatment_id = "f",
    treatment_description = "c",
    richness_type = "f"
  )
) %>%
  filter(
    site == "SW Station",
    richness_type == "seeded_richness",
    !(treatment_id %in% c("2", "4"))
  ) %>%
  mutate(y = establishment_rate)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



graph <- ggplot(
  data = data,
  aes(x = seeded_pool, y = y, fill = seeding_time)
) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .2)) +
  scale_fill_manual(
    breaks = c("fall", "spring"),
    labels = c("Autumn", "Spring"),
    values = c("#440154", "#FFA500")
  ) +
  labs(
    x = "Seeded species pool [#]", fill = "Planting season",
    y = expression(Establishment ~ rate),
    title = "SW Station"
  ) +
  theme_mb();graph

### Save ###
ggsave(
  here("outputs", "figures", "figure_a4c_300dpi_6x4cm.tiff"),
  dpi = 300, width = 6, height = 4, units = "cm"
)

graph_c <- graph +
  guides(fill = FALSE) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.title.y = element_blank()
  ); graph_c
