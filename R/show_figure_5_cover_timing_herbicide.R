#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GREEEN prairie project
# Show figure 5 ####
# Cover non-seeded species ~ herbicide * seeding time
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2025-08-07



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
    year %in% c("2015", "2016", "2017", "2018"),
    richness_type == "seeded_richness",
    treatment_id %in% c("1", "2", "3", "4")
  ) %>%
  mutate(
    treatment = str_c(seeding_time, herbicide, seeded_pool, sep = "_"),
    y = cover_non_seeded,
    treatment = fct_relevel(
      treatment, "unseeded_0_0", "fall_0_33", "spring_0_33", "spring_1_33"
      ),
    treatment = fct_recode(
      treatment, "Unseeded" = "unseeded_0_0", "Fall" = "fall_0_33",
      "Spring" = "spring_0_33", "Spring+\nHerbicide" = "spring_1_33"
    )
  )

### * Model ####
load(file = here("outputs", "models", "model_cover_seeding_time_herbicide_1.Rdata"))
m <- m1
m@call



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



data_model <- ggemmeans(
  m, c("treatment", "site"),
  bias_correction = TRUE,
  back_transform = TRUE
  ) %>%
  as_tibble() %>%
  mutate(
    x = fct_relevel(
      x, "unseeded_0_0", "fall_0_33", "spring_0_33", "spring_1_33"
    ),
    x = fct_recode(
      x, "Unseeded" = "unseeded_0_0", "Fall" = "fall_0_33",
      "Spring" = "spring_0_33", "Spring+\nHerbicide" = "spring_1_33"
    )
  )

data <- sites %>%
  rename(predicted = y, x = treatment, group = site)

data_annotation <- data.frame(
  facet = factor(
    c("NW Station", "Lux Arbor", "SW Station"),
    levels = c("NW Station","Lux Arbor","SW Station")
    ),
  x = c("Unseeded", "Unseeded", "Unseeded"),
  predicted = c(90, 90, 90),
  label = c("n.s.", "n.s.", "n.s.")
  )

(graph_a <- ggplot() +
    geom_errorbar(
      data = data_model,
      aes(x = x, y = predicted, color = x,
          ymin = conf.low, ymax = conf.high),
      width = 0.0, linewidth = 0.4,
      position = position_dodge(width = 0.8)
    ) +
    geom_point(
      data = data_model,
      aes(x, predicted, color = x),
      size = 2, position = position_dodge(width = 0.8)
    ) +
    geom_text(
      data = data_annotation,
      aes(x = x, y = predicted, label = label)
      ) +
    facet_grid(~group) +
    scale_y_continuous(limits = c(0, 90), breaks = seq(0, 100, 10)) +
    scale_color_manual(
      breaks = c("Unseeded" , "Fall", "Spring", "Spring+\nHerbicide"),
      labels = c("Unseeded" , "Fall", "Spring", "Spring+\nHerbicide"),
      values = c("#21918c", "#440154", "#FFA500", "#FFA500"),
      guide = "none"
    ) +
    labs(
      x = "Treatments",
      y = "Cover non-seeded species [%]"
      ) +
    theme_mb() +
    theme(axis.text.x = element_text(angle = 90, vjust = .3)))

### Save ###
ggsave(
  here("outputs", "figures", "figure_5_300dpi_16x8cm.tiff"),
  dpi = 300, width = 16, height = 8, units = "cm"
  )
