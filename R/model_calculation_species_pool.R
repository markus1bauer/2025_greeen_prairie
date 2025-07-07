#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GRASSWORKS Project
# CWMs of EUNIS habitat types ####
# Specific leaf area (SLA) for ESY4
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2025-06-10



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
  here("data", "processed", "data_processed_sites_esy4.csv"),
  col_names = TRUE, na = c("na", "NA", ""), col_types = cols(
    .default = "?",
    eco.id = "f",
    region = col_factor(levels = c("north", "centre", "south"), ordered = TRUE),
    site.type = col_factor(
      levels = c("positive", "restored", "negative"), ordered = TRUE
      ),
    fertilized = "f",
    obs.year = "f"
  )
) %>%
  mutate(esy4 = fct_relevel(esy4, "R", "R22", "R1A")) %>%
  rename(y = cwm.abu.sla)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## 1 Data exploration ##########################################################


### a Graphs of raw data -------------------------------------------------------

ggplot(sites, aes(y = y, x = esy4)) +
  geom_quasirandom(color = "grey") + geom_boxplot(fill = "transparent") +
  labs(y = "CWM SLA (abu) [cm²/g]", x = "Habitat type")

ggplot(sites, aes(y = y, x = eco.id)) +
  geom_quasirandom(color = "grey") +
  geom_boxplot(fill = "transparent") +
  facet_grid(~ esy4) +
  labs(y = "CWM SLA (abu) [cm²/g]", x = "Ecoregion")

ggplot(sites, aes(y = y, x = site.type)) +
  geom_quasirandom(color = "grey") +
  geom_boxplot(fill = "transparent") +
  facet_grid(~ esy4) +
  labs(y = "CWM SLA (abu) [cm²/g]", x = "Site type")

ggplot(sites, aes(y = y, x = obs.year)) +
  geom_quasirandom(color = "grey") +
  geom_boxplot(fill = "transparent") +
  facet_grid(~ esy4) +
  labs(y = "CWM SLA (abu) [cm²/g]", x = "Survey year")


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
