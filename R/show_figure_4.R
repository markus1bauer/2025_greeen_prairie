#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GREEEN prairie project
# Show figure ####
# Seeded species richness ~ herbicide pre-treatment * seeding time
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2025-09-09



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ###############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### Packages ###
library(here)
library(tidyverse)
library(patchwork)

### Start ###
rm(list = setdiff(ls(), c("graph_a", "graph_b", "graph_c", "graph_d",
                          "graph_e", "graph_f")))



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot #######################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



graph_a + graph_b + graph_c + graph_d + graph_e + graph_f +
  plot_layout(
    ncol = 2, guides = "collect", widths = c(1, 1), heights = c(1, 1, 1),
    design = "
    14
    25
    36
    "
    ) +
  plot_annotation(tag_levels = "A", tag_prefix = "", tag_suffix = "") &
  theme(plot.tag = element_text(size = 10, face = "bold"))

### Save ###
ggsave(
  here("outputs", "figures", "figure_4_300dpi_16x24cm.tiff"),
  dpi = 300, width = 16, height = 24, units = "cm"
)
