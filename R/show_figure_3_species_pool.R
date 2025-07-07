#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GRASSWORKS Project
# CWMs of EUNIS habitat types ####
# Show figure of specific leaf area
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2025-05-14



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
  here("data", "processed", "data_processed_sites_esy4.csv"),
  col_names = TRUE, na = c("na", "NA", ""), col_types = cols(
    .default = "?",
    eco.id = col_factor(levels = c("664", "654", "686"), ordered = TRUE),
    site.type = col_factor(
      levels = c("positive", "restored", "negative"), ordered = TRUE
    ),
    fertilized = "f",
    obs.year = "f"
  )
) %>%
  mutate(esy4 = fct_relevel(esy4, "R", "R22", "R1A")) %>%
  rename(y = cwm.abu.sla)

### * Model ####
load(file = here("outputs", "models", "model_sla_esy4_3.Rdata"))
m <- m3
m@call



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## * Preparation ####

data_summary <- sites %>%
  group_by(esy4) %>%
  summarize(mean = mean(y), sd = sd(y, na.rm = TRUE))

data_model <- ggemmeans(
  m, terms = c("esy4"), back.transform = FALSE, ci_level = .95
) %>%
  as_tibble()

### * Plot ####

graph_a <- ggplot() +
  geom_hline(
    yintercept = data_model %>%
      filter(x == "R") %>% select(predicted) %>% pull(),
    linetype = "dashed", color = "grey70", linewidth = .2
  ) +
  geom_quasirandom(
    data = sites,
    aes(x = esy4, y = y, color = esy4),
    alpha = .2, shape = 16, size = 1
  ) +
  geom_boxplot(
    data = sites, aes(x = esy4, y = y, fill = esy4),
    alpha = .5
  ) +
  # geom_errorbar(
  #   data = data_model,
  #   aes(
  #     x = as.numeric(factor(x)) + 0.45, ymin = conf.low, ymax = conf.high,
  #     color = x
  #   ),
  #   width = 0.0, linewidth = 0.4
  # ) +
  # geom_point(
  #   data = data_model,
  #   aes(x = as.numeric(factor(x)) + 0.45, y = predicted, color = x),
  #   size = 1.5
  # ) +
  annotate("text", label = "a", y = 339, x = 1) +
  annotate("text", label = "a", y = 339, x = 2) +
  annotate("text", label = "b", y = 339, x = 3) +
  scale_fill_manual(
    values = c(
      "R" = "#440154",
      "R22" = "#21918c",
      "R1A" = "#FFA500"
    ), guide = "none"
  ) +
  scale_color_manual(
    values = c(
      "R" = "#440154",
      "R22" = "#21918c",
      "R1A" = "#FFA500"
    ), guide = "none"
  ) +
  scale_y_continuous(limits = c(140, 340), breaks = seq(0, 400, 20)) +
  labs(
    x = "",
    y = expression(CWM ~ specific ~ leaf ~ area ~ "[" * cm^2 ~ g^-1 * "]"),
    title = "Specific leaf area",
    tag = "A"
  ) +
  theme_mb() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank()
  ); graph_a

#### * Save ####

ggsave(
  here("outputs", "figures", "figure_3_sla_300dpi_9x6cm.tiff"),
  dpi = 300, width = 9, height = 6, units = "cm"
)
