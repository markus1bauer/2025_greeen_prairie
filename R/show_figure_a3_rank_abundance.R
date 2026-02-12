#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GREEEN prairie project
# Show figure ####
# Rank abundance curve
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2025-09-05



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ###############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### Packages ###
library(here)
library(tidyverse)

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
species <- read_csv(
  here("data", "processed", "data_processed_species.csv"),
  col_names = TRUE, na = c("na", "NA", ""), col_types = cols(.default = "?")
) %>%
  mutate(
    abundance = 1,
    year = year(date_surveyed)
    ) %>%
  filter(
    plot_size %in% c("1", "25"),
    year == "2025",
    str_detect(id_plot, "Lux")
  ) %>%
  group_by(accepted_name) %>%
  count() %>%
  ungroup() %>%
  arrange(desc(n))

traits <- read_csv(
  here("data", "processed", "data_processed_traits.csv"),
  col_names = TRUE, na = c("na", "NA", ""), col_types = cols(.default = "?")
) %>%
  mutate(pool_size = factor(replace_na(pool_size, 0))) %>%
  filter(seeded == "1" & family != "Poaceae")

data <- traits %>%
  select(accepted_name, family, seeded, pool_size) %>%
  left_join(
    species,
    by = "accepted_name"
    ) %>%
  mutate(n = replace_na(n, 0)) %>%
  arrange(desc(n))



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



graph_a <- data %>%
  ggplot(aes(x = reorder(accepted_name, desc(n)), y = n, fill = pool_size)) +
  geom_hline(
    yintercept = c(12, 24, 36, 48),
    color = c("#440154", "#3b528b", "#21918c", "#5ec962"),
    linetype = "dashed"
    ) +
  geom_bar(stat = "identity") +
  annotate(
    "text", size = 3,
    x = c(30, 30, 30, 30), y = c(15, 27, 39, 51),
    label = c("33 (n = 12)", "18 (n = 24)", "12 (n = 36)", "6 (n = 48)"),
    color = c("#440154", "#3b528b", "#21918c", "#5ec962")
    ) +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, 5)) +
  scale_fill_manual(
    values = c("#fde725", "#5ec962", "#21918c", "#3b528b", "#440154")
  ) +
  labs(
    x = "", y = "Number of plots with occurences [#]", fill = "Pool size [#]"
  ) +
  theme_mb() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)); graph_a

### Save ###
ggsave(
  here("outputs", "figures", "figure_a3_300dpi_16x12cm.tiff"),
  dpi = 300, width = 16, height = 12, units = "cm"
  )
