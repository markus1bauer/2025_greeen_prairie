#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GREEEN prairie project
# Show figure ####
# Seeded species richness ~ pool size * seeding time
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2025-07-16



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ###############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### Packages ###
library(here)
library(tidyverse)
library(ggbeeswarm)
library(ggrepel)
library(tidybayes)

### Start ###
rm(list = setdiff(ls(), c("graph_a", "graph_b", "graph_c", "graph_d")))

### Load data ###
sites <- read_csv(
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
      levels = c("6", "12", "18", "33"), ordered = TRUE
    ),
    treatment_id = "f",
    treatment_description = "c",
    richness_type = "f"
  )
) %>%
  filter(
    year %in% c("2015", "2016", "2017", "2018"),
    richness_type == "seeded_richness",
    !(treatment_id %in% c("2", "4"))
  ) %>%
  select(
    id_plot_year, id_plot, site, year, herbicide, seeding_time, seeded_pool,
    richness_1qm, richness_25qm, treatment_id
  ) %>%
  mutate(y = richness_1qm + richness_25qm)

### * Model ####
base::load(file = here("outputs", "models", "model_pool_1_flat.Rdata"))

model <- sites %>%
  tidybayes::add_epred_draws(m1_flat, allow_new_levels = TRUE)

### * Functions ####
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
    legend.text = element_text(size = 9),
    legend.position = "right",
    legend.margin = margin(0, 0, 0, 0, "cm"),
    plot.margin = margin(0, 0, 0, 0, "cm")
  )
}



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## 1 Expectations of predicted draws ###########################################



(graph <- ggplot(data = sites) +
    geom_quasirandom(
      aes(y = y, x = seeded_pool, color = seeding_time),
      alpha = 0.2, shape = 16, cex = .5,
      dodge.width = 0.8
    ) +
    tidybayes::stat_pointinterval(
      aes(y = .epred, x = seeded_pool, color = seeding_time),
      data = model,
      point_interval = median_qi,
      .width = c(.66, .95),
      position = position_dodge(width = 0.8)
    ) +
    facet_grid(~ site) +
    scale_y_continuous(limits = c(0, 16), breaks = seq(0, 20, 1)) +
    scale_color_manual(
      breaks = c("fall", "spring"),
      labels = c("Fall", "Spring"),
      values = c("#440154", "#FFA500")
    ) +
    labs(
      x = "Seeded species pool [#]", fill = "", color = "Seeding",
      y = expression(Seeded ~ species ~ "[" * '#' * "]")
    ) +
    theme_mb())

### Save ###

ggsave(
  here("outputs", "figures", "figure_2_pool_300dpi_16x8cm_bayesian.tiff"),
  dpi = 300, width = 16, height = 8, units = "cm"
)

# (graph_a <- p1 +
#     theme(
#       axis.title.x = element_blank(),
#       axis.text.x = element_blank(),
#       axis.line.x = element_blank(),
#       axis.ticks.x = element_blank()
#     ))
