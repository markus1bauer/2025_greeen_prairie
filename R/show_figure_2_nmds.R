#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GRASSWORKS Project: Bauer et al. (submitted) Habitat type traits
# CWMs of EUNIS habitat types ####
# Show figure of ordination: Non-metric multidimensional scaling (NMDS)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2025-05-22



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ###############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### Packages ###
library(here)
library(tidyverse)
library(vegan)

### Start ###
rm(list = setdiff(ls(), c("graph_a", "graph_b", "graph_c", "graph_d", "ordi")))

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
    plot.margin = margin(0, 0, 0, 0, "cm")
  )
}

vegan_cov_ellipse <- function(cov, center = c(0, 0), scale = 1, npoints = 100) {
  theta <- (0:npoints) * 2 * pi / npoints
  circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(circle %*% chol(cov)))
}

#### Load sites data and model ###

sites <- read_csv(
  here("data", "processed", "data_processed_sites_esy4.csv"),
  col_names = TRUE, na = c("na", "NA", ""), col_types = cols(
    .default = "?",
    obs.year = "f"
  )
) %>%
  mutate(esy4 = fct_relevel(esy4, "R", "R22", "R1A")) %>%
  select(id.plot, id.site, region, eco.id, eco.name, site.type, obs.year, esy4)

sites %>%
  group_by(site.type, esy4) %>%
  count() %>%
  pivot_wider(names_from = site.type, values_from = n)

### Load model ###

base::load(file = here("outputs", "models", "model_plants_nmds_abundance.Rdata"))
ordi_abundance

ordi_points <- ordi_abundance$points %>%
  as.data.frame() %>%
  rownames_to_column("id.plot")



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### 1 Preparation #############################################################


data_nmds <- sites %>%
  inner_join(ordi_points, by = "id.plot") %>%
  select(
    id.plot, MDS1, MDS2, esy4, site.type # Select variables to plot
  ) %>%
  mutate( # modify group
    group_type = str_c(esy4, site.type, sep = "_")
  ) %>% 
  group_by(group_type) %>%
  mutate(
    mean1 = mean(MDS1),
    mean2 = mean(MDS2),
    group_type = factor(group_type)
  )

### * Calculate ellipses ####

ellipses <- tibble()

for (group in levels(data_nmds$group_type)) {
  
  ellipses_calc <- data_nmds %>%
    filter(group_type == group) %>%
    with(
      cov.wt(
        cbind(MDS1, MDS2),
        wt = rep(1 / length(MDS1), length(MDS1))
      )
    )
  
  ellipses <- vegan_cov_ellipse(
    cov = ellipses_calc$cov, center = ellipses_calc$center
  ) %>%
    as_tibble() %>%
    bind_cols(group_type = group) %>%
    bind_rows(ellipses) %>%
    mutate(group_type = factor(group_type))

  data_ellipses <- ellipses %>%
    separate( # separate group type
      group_type, c("esy4", "site.type"),
      sep = "_", remove = FALSE
    ) %>%
    mutate( # fct_relevel groups
      esy4 = fct_relevel(esy4, "R", "R22", "R1A"),
      site.type = fct_relevel(site.type, "positive", "restored", "negative")
      )
}



## 2 Plot #####################################################################


#### * Site scores ####

graph_a <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(
    aes(y = MDS2, x = MDS1, color = site.type),
    data = data_nmds,
    cex = 2, alpha = .5, shape = 16
  ) +
  
  #### * Ellipses ####

  geom_path(
    aes(x = MDS1, y = MDS2, color = site.type),
    data = data_ellipses,
    linewidth = 1, show.legend = FALSE
  ) +
  
  #### * Layout ####

  facet_wrap(~ esy4, nrow = 3) +
  coord_fixed() +
  
  #### * Design ####

  scale_y_continuous(breaks = seq(-1, 1, .5)) +
  scale_x_continuous(breaks = seq(-1, 1, .5)) +
  scale_color_manual(
    values = c(
      positive = "#21918c", restored = "#FFA500", negative = "#440154"
    )
  ) +
  labs(
    x = "NMDS1", y = "NMDS2", fill = "", color = ""
  ) +
  theme_mb() +
  theme(legend.position = "inside", legend.position.inside = c(.15, .08));graph_a

#### * Save ####

ggsave(
  here("outputs", "figures", "figure_2_300dpi_10x18cm.tiff"),
  dpi = 300, width = 10, height = 18, units = "cm"
)
