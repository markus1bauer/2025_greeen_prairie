#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GREEEN prairie project
# Show figure ####
# Seeded species richness ~ herbicide * seeding time
# 2015-2025 only Lux Arbor
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
    site == "Lux Arbor",
    richness_type == "seeded_richness",
    treatment_id %in% c("1", "2", "3", "4")
  ) %>%
  mutate(
    treatment = str_c(seeding_time, herbicide, seeded_pool, sep = "_"),
    y = richness_1qm + richness_25qm,
    treatment = fct_relevel(
      treatment, "unseeded_0_0", "fall_0_33", "spring_0_33", "spring_1_33"
    ),
    treatment = fct_recode(
      treatment, "Unseeded" = "unseeded_0_0", "Fall" = "fall_0_33",
      "Spring" = "spring_0_33", "Spring+\nHerbicide" = "spring_1_33"
    )
  )

### * Model ####
load(file = here("outputs", "models", "model_richness_timing_herbicide_lux_3.Rdata"))
m <- m3
formula(m)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



data_model <- ggemmeans(
  m, c("treatment", "year"),
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
  rename(predicted = y, x = treatment, group = year)

# data_annotation <- data.frame(
#   facet = factor(
#     c("NW Station", "NW Station", "NW Station", "NW Station",
#       "Lux Arbor", "Lux Arbor", "Lux Arbor", "Lux Arbor",
#       "SW Station", "SW Station", "SW Station", "SW Station"),
#     levels = c("NW Station","Lux Arbor","SW Station")
#     ),
#   x = c(.7, 1, 1.3, 2, .7, 1, 1.3, 2, .7, 1, 1.3, 2),
#   group = c("Unseeded", "Fall", "Spring", "Spring", "Unseeded", "Fall",
#             "Spring", "Spring", "Unseeded", "Fall", "Spring", "Spring"),
#   predicted = c(16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16),
#   label = c("c", "a", "bc", "b", "d", "a", "c", "b", "c", "b", "a", "a")
#   )

graph_a <- ggplot() +
    geom_quasirandom(
      data = data,
      aes(x = x, y = predicted, color = x),
      shape = 16, alpha = 0.2, cex = .5,
      dodge.width = 0.8
    ) +
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
    # geom_text(
    #   data = data_annotation,
    #   aes(x = x, y = predicted, label = label)
    #   ) +
    facet_grid(~group) +
    scale_y_continuous(limits = c(0, 20), breaks = seq(0, 21, 5)) +
    scale_color_manual(
      breaks = c("Unseeded" , "Fall", "Spring", "Spring+\nHerbicide"),
      labels = c("Unseeded" , "Fall", "Spring", "Spring+\nHerbicide"),
      values = c("#21918c", "#440154", "#FFA500", "#FFA500"),
      guide = "none"
    ) +
    labs(
      x = "", y = expression(Seeded ~ species ~ "[" * '#' * "]"),
      title = "Lux Arbor"
    ) +
    theme_mb() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0)); graph_a

### Save ###
ggsave(
  here("outputs", "figures", "figure_4_300dpi_16x5cm.tiff"),
  dpi = 300, width = 16, height = 5, units = "cm"
  )
