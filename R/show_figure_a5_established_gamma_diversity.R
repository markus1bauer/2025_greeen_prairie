#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GREEEN prairie project
# Show figure ####
# Target gamma diversity per pool, site and year
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2026-02-12



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
    text = element_text(size = 12, color = "black"),
    strip.text = element_text(size = 12),
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 12, color = "black"),
    axis.line = element_line(),
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
  )


traits <- read_csv(
  here("data", "processed", "data_processed_traits.csv"),
  col_names = TRUE, na = c("na", "NA", ""), col_types = cols(.default = "?")
) %>%
  mutate(pool_size = factor(replace_na(pool_size, 0))) %>%
  filter(seeded == "1" & family != "Poaceae")

sites <- read_csv(
  here("data", "processed", "data_processed_sites.csv"),
  col_names = TRUE, na = c("na", "NA", ""), col_types =
    cols(
      .default = "?",
      seeding_time = "f",
      seeded_pool = col_factor(levels = c("0", "6", "12", "18", "33"), ordered = TRUE)
    ))

data <- traits %>%
  select(accepted_name, seeded) %>%
  left_join(
    species %>%
      ungroup() %>%
      select(accepted_name, id_plot_year),
    by = "accepted_name"
  ) %>%
  left_join(
    sites %>%
      ungroup() %>%
      filter(richness_type == "seeded_richness") %>%
      select(id_plot_year, year, site, seeded_pool),
    by = "id_plot_year"
  ) %>%
  select(accepted_name, site, year, seeded_pool) %>%
  distinct() %>%
  group_by(site, year, seeded_pool) %>%
  count() %>%
  ungroup() %>%
  filter(!(is.na(site)))



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



graph_a <- data %>%
  ggplot(aes(x = seeded_pool, y = n, fill = seeded_pool)) +
  geom_bar(stat = "identity") +
  facet_grid(site ~ year) +
  # scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, 5)) +
  # scale_fill_manual(
  #   values = c("#fde725", "#5ec962", "#21918c", "#3b528b", "#440154")
  # ) +
  labs(
    x = "Seeded pool size [#]", y = "Gamma diversity of seeded forbs [#]"
  ) +
  scale_fill_manual(
    values = c("#fde725", "#5ec962", "#21918c", "#3b528b", "#440154")
  ) +
  guides(fill = "none") +
  theme_mb(); graph_a

### Save ###
ggsave(
  here("outputs", "figures", "figure_a5_300dpi_16x12cm.tiff"),
  dpi = 300, width = 16, height = 12, units = "cm"
)
