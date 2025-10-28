#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GREEEN prairie project
# Model species pool * seeding approach ####
# Seeded species richness
# Lux Arbor
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2025-10-28



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### Packages ###
library(here)
library(tidyverse)
library(ggbeeswarm)
library(patchwork)
library(lme4)
library(glmmTMB)
library(brms)
library(DHARMa)

### Start ###
rm(list = ls())

### Load data ###
sites <- read_csv(
  here("data", "processed", "data_processed_sites.csv"),
  col_names = TRUE, na = c("na", "NA", ""), col_types = cols(
    .default = "?",
    id_plot = "f",
    year = "f",
    seeding_time = col_factor(
      levels = c("unseeded", "fall", "spring"), ordered = FALSE
      ),
    seeded_pool = col_factor(
      levels = c("0", "6", "12", "18", "33"), ordered = TRUE
      ),
    treatment_id = "f",
    richness_type = "f"
  )
) %>%
  filter(
    site == "Lux Arbor",
    richness_type == "seeded_richness",
    !(treatment_id %in% c("2", "4"))
  ) %>%
  mutate(y = richness_1qm + richness_25qm)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## 1 Data exploration ##########################################################


### a Graphs of raw data -------------------------------------------------------

ggplot(sites, aes(y = y, x = year)) +
  geom_quasirandom(color = "grey") +
  geom_boxplot(fill = "transparent") +
  facet_grid(~ site) +
  labs(y = "Seeded species richness (25qm)", x = "Survey year")

ggplot(
  data = sites,
  aes(y = y, x = seeded_pool, fill = seeding_time) # seeding time = herbicide treatment
) +
  geom_quasirandom(aes(color = seeding_time), dodge.width = .8, alpha = .8) +
  geom_boxplot(alpha = .5) +
  facet_grid(~ site) +
  labs(y = "Seeded species richness (25qm)", x = "Species pool [#]")

ggplot(
  data = sites, # herbicide = seeding_time
  aes(y = y, x = seeded_pool, fill = seeding_time, color = seeding_time)
) +
  geom_quasirandom(dodge.width = .8, alpha = .8) +
  geom_boxplot(fill = "transparent") +
  facet_grid(site ~ year) +
  labs(y = "Seeded species richness (25qm)", x = "Species pool [#]")


### b Outliers, zero-inflation, transformations? ------------------------------

sites %>% count(site, year)
sites %>% count(seeded_pool)
sites %>% count(seeding_time)
sites %>% count(herbicide)
plot1 <- ggplot(sites, aes(x = site, y = y)) + geom_quasirandom()
plot2 <- ggplot(sites, aes(x = y)) + geom_histogram(binwidth = 0.7)
plot3 <- ggplot(sites, aes(x = y)) + geom_density()
plot4 <- ggplot(sites, aes(x = sqrt(y))) + geom_density()
(plot1 + plot2) / (plot3 + plot4)


### c Check collinearity Frequency ---------------------------------------------

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

m1 <- glm(
  y ~ seeded_pool + seeding_time + year + water_cap,
  family = poisson(link = "log"),
  data = sites
  )
simulationOutput <- simulateResiduals(m1, plot = TRUE)
testDispersion(simulationOutput)

m2 <- glm(
  y ~ seeded_pool * seeding_time + year + water_cap,
  family = poisson(link = "log"),
  data = sites
  )
simulationOutput <- simulateResiduals(m2, plot = TRUE)
testDispersion(simulationOutput)

m3 <- glmmTMB::glmmTMB(
  y ~ seeded_pool * seeding_time + year + water_cap,
  family = nbinom2(link = "log"),
  data = sites
)
simulationOutput <- simulateResiduals(m3, plot = TRUE)
testDispersion(simulationOutput)

m4 <- glmmTMB::glmmTMB(
  y ~ seeded_pool * seeding_time + year,
  family = nbinom2(link = "log"),
  data = sites
)
simulationOutput <- simulateResiduals(m4, plot = TRUE)
testDispersion(simulationOutput)


### b Save ---------------------------------------------------------------------

save(m2, file = here("outputs", "models", "model_richness_pool_lux_2.Rdata"))
save(m3, file = here("outputs", "models", "model_richness_pool_lux_3.Rdata"))
