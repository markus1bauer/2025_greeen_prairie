#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GREEEN prairie project
# Show figure ####
# Seeded species richness ~ herbicide * seeding time
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2025-07-16



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
  select(
    id_plot_year, id_plot, site, year, herbicide, seeding_time, seeded_pool,
    richness_1qm, richness_25qm, treatment_id
  ) %>%
  mutate(y = richness_1qm + richness_25qm)

### * Model ####
load(file = here("outputs", "models", "model_seeding_time_herbicide_full.Rdata"))
m <- m_full
m@call



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



data_model <- ggemmeans(
  m, c("herbicide", "seeding_time", "site"),
  bias_correction = TRUE
  ) %>%
  as_tibble() %>%
  mutate(x = fct_recode(x, "No" = "0", "Yes" = "1")) %>%
  filter(!(is.na(predicted)))

data <- sites %>%
  rename(predicted = y, x = herbicide, group = seeding_time, facet = site) %>%
  mutate(x = fct_recode(x, "No" = "0", "Yes" = "1"))

data_annotation <- data.frame(
  facet = factor(
    c("NW Station", "NW Station", "NW Station", "NW Station",
      "Lux Arbor", "Lux Arbor", "Lux Arbor", "Lux Arbor",
      "SW Station", "SW Station", "SW Station", "SW Station"),
    levels = c("NW Station","Lux Arbor","SW Station")
    ),
  x = c(.7, 1, 1.3, 2, .7, 1, 1.3, 2, .7, 1, 1.3, 2),
  group = c("Unseeded", "Fall", "Spring", "Spring", "Unseeded", "Fall",
            "Spring", "Spring", "Unseeded", "Fall", "Spring", "Spring"),
  predicted = c(16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16),
  label = c("c", "a", "bc", "b", "d", "a", "c", "b", "c", "b", "a", "a")
  )

(graph_a <- ggplot() +
    geom_quasirandom(
      data = data,
      aes(x = x, y = predicted, color = group),
      shape = 16, alpha = 0.2, cex = .5,
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
    geom_text(
      data = data_annotation,
      aes(x = x, y = predicted, label = label)
      ) +
    facet_grid(~facet) +
    scale_y_continuous(limits = c(0, 16), breaks = seq(0, 20, 1)) +
    scale_color_manual(
      breaks = c("unseeded", "fall", "spring"),
      labels = c("Unseeded" , "Fall", "Spring"),
      values = c("#21918c", "#440154", "#FFA500")
    ) +
    labs(
      x = "Extra herbicide pre-treatment", color = "Seeding",
      y = expression(Seeded ~ species ~ "[" * '#' * "]")
      ) +
    theme_mb())

### Save ###
ggsave(
  here("outputs", "figures", "figure_3_300dpi_16x8cm.tiff"),
  dpi = 300, width = 16, height = 8, units = "cm"
  )
