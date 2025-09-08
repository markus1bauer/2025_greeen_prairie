#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GREEEN prairie project
# Show figure ####
# Seeded species richness ~ pool size * seeding time
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2025-09-05



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
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    legend.margin = margin(0, 0, 0, 0, "cm"),
    plot.margin = margin(0, 0, 0, 0, "cm"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
}

#### Load data ###
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
      levels = c("0", "6", "12", "18", "33"), ordered = TRUE
    ),
    treatment_id = "f",
    treatment_description = "c",
    richness_type = "f"
  )
) %>%
  filter(
    site == "Lux Arbor",
    richness_type == "seeded_richness",
    !(treatment_id %in% c("2", "4"))
  ) %>%
  mutate(y = richness_1qm + richness_25qm)

### * Model ####
load(file = here("outputs", "models", "model_richness_pool_lux_3.Rdata"))
m <- m3
formula(m)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



data_model <- ggemmeans(
  m, c("seeded_pool", "seeding_time", "year"),
  back_transform = TRUE
  ) 

data <- sites %>%
  rename(predicted = y, x = seeded_pool, group = seeding_time, facet = year)

(graph_a <- ggplot() +
    geom_quasirandom(
      data = data,
      aes(x = x, y = predicted, color = group),
      alpha = 0.2, shape = 16, cex = .5,
      dodge.width = 0.8
    ) +
    geom_errorbar(
      data = data_model,
      aes(x = x, y = predicted, color = group,
          ymin = conf.low, ymax = conf.high),
      width = 0.0, linewidth = 0.4,
      position = position_dodge(width = 0.8)
    ) +
    geom_point(
      data = data_model,
      aes(x, predicted, color = group),
      size = 2, position = position_dodge(width = 0.8)
    ) +
    facet_grid(~facet) +
    scale_y_continuous(limits = c(0, 18), breaks = seq(0, 21, 5)) +
    scale_color_manual(
      breaks = c("fall", "spring"),
      labels = c("Fall", "Spring"),
      values = c("#440154", "#FFA500")
    ) +
    labs(
      x = "Seeded species pool [#]", color = "Seeding",
      y = expression(Seeded ~ species ~ "[" * '#' * "]"),
      title = "Lux Arbor"
      ) +
    theme_mb())

### Save ###
ggsave(
  here("outputs", "figures", "figure_2_300dpi_16x5cm.tiff"),
  dpi = 300, width = 16, height = 5, units = "cm"
  )
