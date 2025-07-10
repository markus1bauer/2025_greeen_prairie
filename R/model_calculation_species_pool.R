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
      levels = c("NW Station", "Lux Arbor", "SW Station")
    ),
    seeding_time = col_factor(
      levels = c("unseeded", "fall", "spring"), ordered = TRUE
      ),
    herbicide = col_factor(levels = c("0", "1"), ordered = TRUE),
    seeded_pool = col_factor(
      levels = c("0", "6", "12", "18", "33"), ordered = TRUE
      ),
    year = "f"
  )
) %>%
  filter(
    year %in% c("2015", "2016", "2017", "2018"),
    richness_type == "seeded_richness",
    !(treatment_id %in% c("2", "4"))
  ) %>%
  select(
    id_plot_year, id_plot, site, year, herbicide, seeding_time, seeded_pool,
    richness_1qm, richness_25qm, treatment_id
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
  sites, aes(y = y, x = year, fill = seeding_time) # herbicide = seeding_time
  ) +
  geom_quasirandom(aes(color = seeding_time), dodge.width = .8, alpha = .8) +
  geom_boxplot(alpha = .3) +
  facet_grid(~ site) +
  labs(y = "Seeded species richness (25qm)", x = "Herbicide treatment")

ggplot(
  sites, aes(y = y, x = year, fill = seeded_pool)
  ) +
  geom_quasirandom(aes(color = seeded_pool), dodge.width = .8, alpha = .8) +
  geom_boxplot(alpha = .3) +
  facet_grid(~ site) +
  labs(y = "Seeded species richness (25qm)", x = "Species pool")

ggplot(
  data = sites,
  aes(y = y, x = seeded_pool, fill = seeding_time)
  ) +
  geom_quasirandom(aes(color = seeding_time), dodge.width = .8, alpha = .8) +
  geom_boxplot(alpha = .3) +
  facet_grid(~ year) +
  labs(y = "Seeded species richness (25qm)", x = "Species pool")

ggplot(
  data = sites,
  aes(y = y, x = seeded_pool, fill = seeding_time)
  ) +
  geom_quasirandom(aes(color = seeding_time), dodge.width = .8, alpha = .8) +
  geom_boxplot(alpha = .5) +
  facet_grid(~ site) +
  labs(y = "Seeded species richness (25qm)", x = "Species pool")

ggplot(
  data = sites,
  aes(y = y, x = seeded_pool, fill = seeding_time)
) +
  geom_quasirandom(aes(color = seeding_time), dodge.width = .8, alpha = .8) +
  geom_boxplot(alpha = .5) +
  facet_grid(site ~ year) +
  labs(y = "Seeded species richness (25qm)", x = "Species pool")

### b Outliers, zero-inflation, transformations? ------------------------------

sites %>% count(site, year)
sites %>% count(seeded_pool)
sites %>% count(seeding_time)
sites %>% count(herbicide)
plot1 <- ggplot(sites, aes(x = site, y = y)) + geom_quasirandom()
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

m_simple <- lmer(
  y ~ seeded_pool + herbicide + site + (1 | year),
  REML = FALSE,
  data = sites
  )
simulateResiduals(m_simple, plot = TRUE)
m_full <- glmer(
  y ~ seeded_pool * herbicide * site + (1 | year),
  family = poisson(link = "log"),
  data = sites
  )
simulateResiduals(m_full, plot = TRUE)
m1 <- lmer(
  y ~ seeded_pool * herbicide + site + (1 | year),
  REML = FALSE,
  data = sites
)
simulateResiduals(m1, plot = TRUE)


### b Save ---------------------------------------------------------------------

save(m1, file = here("outputs", "models", "model_sla_esy4_1.Rdata"))
save(m2, file = here("outputs", "models", "model_sla_esy4_2.Rdata"))
save(m3, file = here("outputs", "models", "model_sla_esy4_3.Rdata"))


## 2 Model building ###########################################################


### a Possible priors ----------------------------------------------------------

get_prior(
  n ~ species_pool * herbicide * year + (1 | site), 
  data = sites
  )
data <- data.frame(x = c(-5, 5))
ggplot(data, aes(x = x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 2)) +
  stat_function(fun = dnorm, xlim = c(min(data$x), q025), geom = "area") +
  expand_limits(y = 0) +
  ggtitle("Normal distribution for Intercept")
ggplot(data, aes(x = x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0.3, sd = 2)) +
  expand_limits(y = 0) +
  ggtitle("Normal distribution for treatments")
ggplot(data, aes(x = x)) +
  stat_function(fun = dcauchy, n = 101, args = list(location = 0, scale = 1)) +
  expand_limits(y = 0) +
  ggtitle("Cauchy distribution")
ggplot(data, aes(x = x)) +
  stat_function(fun = dstudent_t, args = list(df = 3, mu = 0, sigma = 2.5)) +
  expand_limits(y = 0) +
  ggtitle(expression(Student ~ italic(t) * "-distribution"))


### b Model specifications -----------------------------------------------------

# NUTS sampler used
iter <- 10000
chains <- 4
thin <- 2
seed <- 123
warmup <- floor(iter / 2)
priors <- c(
  set_prior("normal(0, 2)", class = "Intercept"),
  set_prior("normal(0, 2)", class = "b"),
  set_prior("normal(0.1, 2)", class = "b", coef = "species_pool12"),
  set_prior("normal(0.2, 2)", class = "b", coef = "species_pool18"),
  set_prior("normal(0.3, 2)", class = "b", coef = "species_pool33"),
  set_prior("normal(0.1, 2)", class = "b", coef = "herbicide1"),
  set_prior("normal(0.1, 2)", class = "b", coef = "year2016"),
  set_prior("normal(0.2, 2)", class = "b", coef = "year2017"),
  set_prior("normal(0.3, 2)", class = "b", coef = "year2018"),
  set_prior("cauchy(0, 1)", class = "sigma")
)


### c Models ------------------------------------------------------------------

m_simple <- brm(
  y ~ seeded_pool + herbicide + site + (1 | year),
  data = sites,
  family = gaussian("identity"),
  prior = priors,
  chains = chains,
  iter = iter,
  thin = thin,
  control = list(max_treedepth = 13),
  warmup = warmup,
  save_pars = save_pars(all = TRUE),
  cores = parallel::detectCores(),
  seed = seed
)

m_full <- brm(
  y ~ seeded_pool * herbicide * site + (1 | year),
  data = sites,
  family = gaussian("identity"),
  prior = priors,
  chains = chains,
  iter = iter,
  thin = thin,
  control = list(max_treedepth = 13),
  warmup = warmup,
  save_pars = save_pars(all = TRUE),
  cores = parallel::detectCores(),
  seed = seed
)

m1 <- brm(
  y ~ seeded_pool * herbicide + site + (1 | year),
  data = sites,
  family = gaussian("identity"),
  prior = priors,
  chains = chains,
  iter = iter,
  thin = thin,
  control = list(max_treedepth = 13),
  warmup = floor(iter / 2),
  save_pars = save_pars(all = TRUE),
  cores = parallel::detectCores(),
  seed = seed
)

m1_flat <- brm(
  y ~ seeded_pool * herbicide + site + (1 | year),
  data = sites,
  family = poisson,
  # prior = c(
  #   set_prior("normal(0, 4)", class = "Intercept"),
  #   set_prior("normal(0, 4)", class = "b"),
  #   set_prior("cauchy(0, 1)", class = "sigma")
  # ),
  chains = chains,
  iter = iter,
  thin = thin,
  control = list(max_treedepth = 13),
  warmup = warmup,
  save_pars = save_pars(all = TRUE),
  cores = parallel::detectCores(),
  seed = seed
)

m1_prior <- brm(
  y ~ seeded_pool * herbicide + site + (1 | site),
  data = sites,
  family = gaussian("identity"),
  prior = priors,
  sample_prior = "only",
  chains = chains,
  iter = iter,
  thin = thin,
  control = list(max_treedepth = 13),
  warmup = warmup,
  save_pars = save_pars(all = TRUE),
  cores = parallel::detectCores(),
  seed = seed
)


### d Save ---------------------------------------------------------------------

save(m_simple, file = here("outputs", "models", "model_pool_simple.Rdata"))
save(m_full, file = here("outputs", "models", "model_pool_full.Rdata"))
save(m1, file = here("outputs", "models", "model_pool_1.Rdata"))
save(m1_flat, file = here("outputs", "models", "model_pool_1_flat.Rdata"))
save(m1_prior, file = here("outputs", "models", "model_pool_1_prior.Rdata"))
