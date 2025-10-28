#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GREEEN prairie project
# Model seeding time * extra herbicide pre-treatment ####
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
    herbicide = col_factor(levels = c("0", "1"), ordered = FALSE),
    treatment_id = "f",
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
    treatment = factor(treatment)
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
  facet_grid(~site) +
  labs(y = "Cover ratio (1qm) [%]", x = "Survey year")

ggplot(
  data = sites,
  aes(y = y, x = treatment, fill = treatment)
) +
  geom_quasirandom(dodge.width = .8, alpha = .8) +
  geom_boxplot(alpha = .5) +
  facet_grid(~site) +
  labs(y = "Cover (1qm) [%]", x = "Survey year")

ggplot(
  data = sites,
  aes(y = y, x = treatment, fill = treatment)
) +
  geom_quasirandom(dodge.width = .8, alpha = .8) +
  geom_boxplot(alpha = .5) +
  facet_grid(site~year) +
  labs(y = "Cover (1qm) [%]", x = "Survey year")

ggplot(data = sites, aes(y = y, x = water_cap)) +
  geom_point() +
  geom_smooth(span = 2) +
  labs(y = "Cover (1qm) [%]", x = "Water capacity [%]")


### b Outliers, zero-inflation, transformations? ------------------------------

sites %>% count(site, year)
sites %>% count(treatment)
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
  y ~ treatment * year + water_cap,
  family = poisson(link = "log"),
  data = sites
  )
simulationOutput <- simulateResiduals(m1, plot = TRUE)
testDispersion(simulationOutput)

m2 <- glmmTMB::glmmTMB(
  y ~ treatment * year,
  family = nbinom2(link = "log"),
  data = sites
)
simulationOutput <- simulateResiduals(m2, plot = TRUE)
testDispersion(simulationOutput)

m3 <- lm(
  y ~ treatment * year + water_cap,
  data = sites
)
simulateResiduals(m3, plot = TRUE)

m4 <- lm(
  y ~ treatment * (year + water_cap),
  data = sites
)
simulateResiduals(m4, plot = TRUE)


### b Save ---------------------------------------------------------------------

save(m1, file = here("outputs", "models", "model_richness_timing_herbicide_lux_1.Rdata"))
save(m2, file = here("outputs", "models", "model_richness_timing_herbicide_lux_2.Rdata"))
