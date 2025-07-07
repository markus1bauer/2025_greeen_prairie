#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GRASSWORKS Project
# NMDS: Non-metric multidimensional scaling ordination ####
# Plants
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2024-08-12



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### Packages ###
library(here)
library(tidyverse)
library(vegan)

### Start ###
rm(list = ls())

#### * Load data sites ####

data <- read_csv(
  here("data", "processed", "sites_processed_environment_nms_20240812.csv"),
  col_names = TRUE, na = c("na", "NA", ""), col_types = cols(
    .default = "?"
  )
) %>%
  select(
    id.plot, veg.height, biomass.1, eco.id, eco.name, esy4, esy16,
    longitude, latitude,
    fdis.abu.oek.f, cwm.abu.oek.f, obs.year, site.type, history, changes,
    region
  )

data %>%
  group_by(esy16) %>%
  count()

sites <- data %>%
  mutate(
    esy.simple4 = if_else(str_detect(esy4, "V"), "Other", esy4),
    esy.simple4 = if_else(str_detect(esy4, "Q"), "Other", esy.simple4),
    esy.simple4 = if_else(str_detect(esy4, "Sa"), "Other", esy.simple4),
    esy.simple4 = if_else(str_detect(esy4, "\\?"), "Other", esy.simple4),
    esy.simple4 = if_else(str_detect(esy4, "R1"), "R1", esy.simple4),
    esy.simple4 = if_else(str_detect(esy4, "R2"), "R2", esy.simple4),
    esy.simple4 = if_else(str_detect(esy4, "R3"), "R3", esy.simple4),
    esy.simple = if_else(str_detect(esy16, "\\?"), "Other", esy16),
    esy.simple = if_else(str_detect(esy16, "R1"), "R1", esy.simple),
    esy.simple = if_else(str_detect(esy16, "R2"), "R2", esy.simple),
    esy.simple = if_else(str_detect(esy16, "R3"), "R3", esy.simple)
  ) %>%
  drop_na(esy.simple)

sites %>%
  group_by(esy.simple) %>%
  count()

sites %>%
  group_by(esy.simple4) %>%
  count()

#### * Load data species ####

data <- data.table::fread(
  here(
    "data", "processed", "data_processed_species_plants_20240808.csv"
  ),
  sep = ",",
  dec = ".",
  skip = 0,
  header = TRUE,
  na.strings = c("", "NA", "na")
) %>%
  filter(!str_detect(id.plot, "_T$"))
  
### Each plot should have a summed vegetation cover of >5 % ###
data_plot <- data %>%
  group_by(id.plot) %>%
  summarise(sum = sum(cover, na.rm = TRUE)) %>%
  filter(sum > 5)

### Every species should have a cover >1 % over all plots ###
data_species <- data %>%
  group_by(name.plant) %>%
  summarise(sum = sum(cover, na.rm = TRUE)) %>%
  filter(sum > 1)

### Finalize species table for processing ####
species <- data %>%
  semi_join(data_species, by = "name.plant") %>%
  semi_join(data_plot, by = "id.plot") %>%
  semi_join(sites, by = "id.plot") %>%
  mutate(name.plant = str_replace_all(name.plant, " ", "_")) %>%
  pivot_wider(names_from = "name.plant", values_from = "cover") %>%
  mutate(across(where(is.numeric), ~replace(., is.na(.), 0))) %>%
  arrange(id.plot)
  
sites <- sites %>%
  semi_join(species, by = "id.plot") %>%
  arrange(id.plot)

species <- species %>%
  column_to_rownames("id.plot")

rm(list = setdiff(ls(), c("sites", "species", "species2", "theme_mb")))



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## 1 NMDS #####################################################################


### a Abundance data ----------------------------------------------------------

### Calculate ### try several times
# ordi_abundance <- metaMDS(
#   species, dist = "bray", k = 2, binary = FALSE,
#   try = 50, trymax = 51, previous.best = TRUE, na.rm = TRUE
#   )
base::load(here("outputs", "models", "model_nmds_abundance.Rdata"))
ordi_abundance

### Stress ###
stressplot(ordi_abundance)
goodness_of_fit <- goodness(ordi_abundance)
plot(ordi_abundance, type = "t", main = "Goodness of fit")
points(ordi_abundance, display = "sites", cex = goodness_of_fit * 300)


### b Presence-absence data ----------------------------------------------------

### Calculate ### try several times
# ordi_presence <- metaMDS(
#   species, dist = "bray", k = 2, binary = TRUE,
#   try = 50, trymax = 51, previous.best = TRUE, na.rm = TRUE
# )
base::load(here("outputs", "models", "model_nmds_presence.Rdata"))
ordi_presence

### Stress ###
stressplot(ordi_presence)
goodness_of_fit <- goodness(ordi_presence)
plot(ordi_presence, type = "t", main = "Goodness of fit")
points(ordi_presence, display = "sites", cex = goodness_of_fit * 300)


## 2 Environmental factors ####################################################


#### a Vectors ----------------------------------------------------------------

(ef_vector1 <- envfit(
  ordi_presence ~ latitude + longitude + cwm.abu.oek.f + veg.height,
  data = sites,
  permu = 999,
  na.rm = TRUE
  ))
plot(ordi_presence, type = "n")
plot(ef_vector1, add = TRUE, p. = .99)
(ef_vector2 <- envfit(
  ordi_presence ~ latitude + cwm.abu.oek.f,
  data = sites,
  permu = 999,
  na.rm = TRUE
  ))
plot(ordi_presence, type = "n")
plot(ef_vector2, add = TRUE, p. = .99)


#### b Factors ----------------------------------------------------------------

(ef_factor1 <- envfit(
  ordi_presence ~  esy.simple + site.type,
  data = sites, permu = 999, na.rm = TRUE
  ))
plot(ordi_presence, type = "n")
plot(ef_vector2, add = TRUE, p. = .99)
ordiellipse(ordi_presence, sites$esy.simple, kind = "sd", draw = "lines", label = TRUE)
plot(ordi_presence, type = "n")
plot(ef_vector2, add = TRUE, p. = .99)
ordiellipse(ordi_presence, sites$site.type, kind = "sd", draw = "lines", label = TRUE)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# C Save ######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



save(
  ordi_abundance,
  file = here("outputs", "models", "model_plants_nmds_abundance.Rdata")
  )
save(
  ordi_presence,
  file = here("outputs", "models", "model_plants_nmds_presence.Rdata")
  )
save(
  ef_vector2,
  file = here("outputs", "models", "model_plants_nmds_envfit_vector.Rdata")
  )
