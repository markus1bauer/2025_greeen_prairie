#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GREEEN prairie project
# Model species pool * seeding approach ####
# Seeded species richness
# 2015-2025 only Lux Arbor
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
library(brms)
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
    site = col_factor(
      levels = c("NW Station", "Lux Arbor", "SW Station"), ordered = FALSE
    ),
    year = "f",
    seeding_time = col_factor(
      levels = c("unseeded", "fall", "spring"), ordered = FALSE
      ),
    herbicide = col_factor(levels = c("0", "1"), ordered = FALSE),
    seeded_pool = col_factor(
      levels = c("0", "6", "12", "18", "33"), ordered = TRUE
      ),
    treatment_id = "f",
    treatment_description = "c",
    richness_type = "f"
  )
) %>%
  filter(
    year != "2015",
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


### c Check collinearity Frequency ------------------------------------------------------

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

m_simple <- glm(
  y ~ seeded_pool + seeding_time + year,
  family = poisson(link = "log"),
  data = sites
  )
simulateResiduals(m_simple, plot = TRUE)
m_full <- glm(
  y ~ seeded_pool * seeding_time * year + water_cap,
  family = poisson(link = "log"),
  data = sites
  )
simulateResiduals(m_full, plot = TRUE)
m1 <- lm(
  sqrt(y) ~ seeded_pool * seeding_time * year,
  data = sites
)
simulateResiduals(m1, plot = TRUE)


### b Save ---------------------------------------------------------------------

# save(m_simple, file = here("outputs", "models", "model_pool_simple.Rdata")) # bad model critique
save(m_full, file = here("outputs", "models", "model_richness_pool_long_full.Rdata"))
save(m1, file = here("outputs", "models", "model_richness_pool_long_1.Rdata"))


## 2 Model building Bayesian ###########################################################


### a Possible priors ----------------------------------------------------------

# brms::get_prior(
#   y ~ seeded_pool * seeding_time * site + (1 | year), 
#   data = sites
#   )
# data <- data.frame(x = c(-10, 10))
# ggplot(data, aes(x = x)) +
#   stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 10)) +
#   expand_limits(y = 0) +
#   ggtitle("Normal distribution for Intercept")
# ggplot(data, aes(x = x)) +
#   stat_function(fun = dnorm, n = 101, args = list(mean = 0.5, sd = 10)) +
#   expand_limits(y = 0) +
#   ggtitle("Normal distribution for treatments")
# ggplot(data, aes(x = x)) +
#   stat_function(fun = dcauchy, n = 101, args = list(location = 0, scale = 1)) +
#   expand_limits(y = 0) +
#   ggtitle("Cauchy distribution")
# ggplot(data, aes(x = x)) +
#   stat_function(fun = dstudent_t, args = list(df = 3, mu = 0, sigma = 2.5)) +
#   expand_limits(y = 0) +
#   ggtitle(expression(Student ~ italic(t) * "-distribution"))
# 
# 
# ### b Model specifications -----------------------------------------------------
# 
# # NUTS sampler used
# iter <- 10000
# chains <- 4
# thin <- 2
# seed <- 123
# warmup <- floor(iter / 2)
# priors <- c(
#   set_prior("normal(0, 10)", class = "Intercept"),
#   set_prior("normal(0, 10)", class = "b"),
#   # set_prior("normal(0.15, 10)", class = "b", coef = "seeded_pool12"),
#   # set_prior("normal(0.25, 10)", class = "b", coef = "seeded_pool18"),
#   # set_prior("normal(0.5, 10)", class = "b", coef = "seeded_pool33"),
#   # set_prior("normal(0.5, 10)", class = "b", coef = "seeding_time1"),
#   set_prior("cauchy(0, 1)", class = "sigma")
# )
# 
# 
# ### c Models ------------------------------------------------------------------
# 
# m_simple <- brm(
#   y ~ seeded_pool + seeding_time + site + (1 | year),
#   data = sites,
#   family = gaussian("identity"),
#   prior = priors,
#   chains = chains,
#   iter = iter,
#   thin = thin,
#   control = list(max_treedepth = 13),
#   warmup = warmup,
#   save_pars = save_pars(all = TRUE),
#   cores = parallel::detectCores(),
#   seed = seed
# )
# 
# m_full <- brm(
#   y ~ seeded_pool * seeding_time * site + (1 | year),
#   data = sites,
#   family = gaussian("identity"),
#   prior = priors,
#   chains = chains,
#   iter = iter,
#   thin = thin,
#   control = list(max_treedepth = 13),
#   warmup = warmup,
#   save_pars = save_pars(all = TRUE),
#   cores = parallel::detectCores(),
#   seed = seed
# )
# 
# m1 <- brm(
#   y ~ seeded_pool * seeding_time + site + (1 | year),
#   data = sites,
#   family = gaussian("identity"),
#   prior = priors,
#   chains = chains,
#   iter = iter,
#   thin = thin,
#   control = list(max_treedepth = 13),
#   warmup = floor(iter / 2),
#   save_pars = save_pars(all = TRUE),
#   cores = parallel::detectCores(),
#   seed = seed
# )
# 
# m1_flat <- brm(
#   y ~ seeded_pool * seeding_time * site + (1 | year),
#   data = sites,
#   family = poisson,
#   prior = c(
#     set_prior("normal(0, 10)", class = "Intercept"),
#     set_prior("normal(0, 10)", class = "b")#,
#     #set_prior("cauchy(0, 1)", class = "sigma")
#   ),
#   chains = chains,
#   iter = iter,
#   thin = thin,
#   warmup = warmup,
#   save_pars = save_pars(all = TRUE),
#   cores = parallel::detectCores(),
#   seed = seed
# )
# 
# m1_prior <- brm(
#   y ~ seeded_pool * seeding_time * site + (1 | site),
#   data = sites,
#   family = gaussian("identity"),
#   prior = priors,
#   sample_prior = "only",
#   chains = chains,
#   iter = iter,
#   thin = thin,
#   warmup = warmup,
#   cores = parallel::detectCores(),
#   seed = seed
# )
# 
# 
# ### d Save ---------------------------------------------------------------------
# 
# save(
#   m_simple, file = here("outputs", "models", "model_pool_simple_bayesian.Rdata")
#   )
# save(m_full, file = here("outputs", "models", "model_pool_full_bayesian.Rdata"))
# save(m1, file = here("outputs", "models", "model_pool_1_bayesian.Rdata"))
# save(
#   m1_flat, file = here("outputs", "models", "model_pool_1_flat_bayesian.Rdata")
#   )
# save(
#   m1_prior,
#   file = here("outputs", "models", "model_pool_1_prior_bayesian.Rdata")
#   )
