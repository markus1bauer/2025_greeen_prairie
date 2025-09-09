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
library(patchwork)

### Start ###
rm(list = setdiff(ls(), c("graph_a", "graph_b", "graph_c", "graph_d")))



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot #######################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



graph_a / (graph_b + graph_c) +
  plot_layout(guides = "collect") +
  plot_annotation(
    tag_levels = "A", tag_prefix = "", tag_suffix = "",
    theme = theme(legend.position = "bottom")
    ) &
  theme(plot.tag = element_text(size = 10, face = "bold"))

### Save ###
ggsave(
  here("outputs", "figures", "figure_2_300dpi_12x10cm.tiff"),
  dpi = 300, width = 12, height = 10, units = "cm"
)
