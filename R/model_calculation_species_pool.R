#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GREEEN prairie project
# Model species pool * seeding approach ####
# Seeded species richness
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2025-07-09



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### Packages ###
library(here)
library(tidyverse)
library(ggbeeswarm)
library(patchwork)
library(lme4)
library(DHARMa)

### Start ###
rm(list = ls())

### Load data ###
sites <- read_csv(
  here("data", "processed", "data_processed_sites.csv"),
  col_names = TRUE, na = c("na", "NA", ""), col_types = cols(
    .default = "?",
    id_plot_year = "f",
    id_plot = "f",
    site = "f",
    treatment_id = "f",
    seeding_time = col_factor(
      levels = c("unseeded", "fall", "spring"), ordered = TRUE
      ),
    herbicide = col_factor(levels = c("0", "1"), ordered = TRUE),
    species_pool = col_factor(
      levels = c("0", "6", "12", "18", "33"), ordered = TRUE
      ),
    year = "f"
  )
) %>%
  filter(
    year %in% c("2015", "2016", "2017", "2018"),
    richness_type == "seeded_richness",
    pool_1 > 0,
    !(treatment_id %in% c("2", "4"))
  ) %>%
  select(-pool_25, -pool_seeded) %>%
  mutate(y = pool_1)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## 1 Data exploration ##########################################################


### a Graphs of raw data -------------------------------------------------------

ggplot(sites, aes(y = y, x = year)) +
  geom_quasirandom(color = "grey") +
  geom_boxplot(fill = "transparent") +
  facet_grid(~ site) +
  labs(y = "Seeded species richness", x = "Survey year")

ggplot(sites, aes(y = y, x = year, color = herbicide)) +
  geom_quasirandom(color = "grey") +
  geom_boxplot(fill = "transparent") +
  facet_grid(~ site) +
  labs(y = "Seeded species richness", x = "Herbicide treatment")

ggplot(sites, aes(y = y, x = year, color = seeding_time)) +
  geom_quasirandom(color = "grey") +
  geom_boxplot(fill = "transparent") +
  facet_grid(~ site) +
  labs(y = "Seeded species richness", x = "Seeding time")

ggplot(sites, aes(y = y, x = year, color = species_pool)) +
  #geom_quasirandom() +
  geom_boxplot(fill = "transparent") +
  facet_grid(~ site) +
  labs(y = "Seeded species richness", x = "Species pool")

ggplot(sites, aes(y = y, x = species_pool, color = seeding_time)) +
  #geom_quasirandom(color = "grey") +
  geom_boxplot(fill = "transparent") +
  facet_grid(~ year) +
  labs(y = "Seeded species richness", x = "Species pool")

ggplot(sites, aes(y = y, x = species_pool, color = seeding_time)) +
  #geom_quasirandom(color = "grey") +
  geom_boxplot(fill = "transparent") +
  facet_grid(~ site) +
  labs(y = "Seeded species richness", x = "Species pool")

### b Outliers, zero-inflation, transformations? ------------------------------

sites %>% count(eco.id)
sites %>% count(site.type)
sites %>% count(esy4)
sites %>% count(esy4, eco.id)
sites %>% count(esy4, site.type)
plot1 <- ggplot(sites, aes(x = region, y = y)) + geom_quasirandom()
plot2 <- ggplot(sites, aes(x = y)) + geom_histogram(binwidth = 0.7)
plot3 <- ggplot(sites, aes(x = y)) + geom_density()
plot4 <- ggplot(sites, aes(x = log(y))) + geom_density()
(plot1 + plot2) / (plot3 + plot4)


### c Check collinearity ------------------------------------------------------

# sites %>%
#   select() %>%
#   GGally::ggpairs(lower = list(continuous = "smooth_loess")) +
#   theme(strip.text = element_text(size = 7))
#--> exclude r > 0.7
# Dormann et al. 2013 Ecography
# https://doi.org/10.1111/j.1600-0587.2012.07348.x

# -> No continuous explanatory variables



## 2 Model building ###########################################################


### a Candidate models ---------------------------------------------------------

m1 <- lmer(
  y ~ esy4 * (site.type + eco.id) + obs.year + (1|id.site),
  REML = FALSE,
  data = sites
  )
simulateResiduals(m1, plot = TRUE)
m2 <- lmer(
  y ~ esy4 * site.type + eco.id + obs.year + (1|id.site),
  REML = FALSE,
  data = sites
  )
simulateResiduals(m2, plot = TRUE)
m3 <- lmer(
  y ~ esy4 * site.type + eco.id + obs.year + hydrology + (1|id.site),
  REML = FALSE,
  data = sites
)
simulateResiduals(m3, plot = TRUE)


### b Save ---------------------------------------------------------------------

save(m1, file = here("outputs", "models", "model_sla_esy4_1.Rdata"))
save(m2, file = here("outputs", "models", "model_sla_esy4_2.Rdata"))
save(m3, file = here("outputs", "models", "model_sla_esy4_3.Rdata"))
