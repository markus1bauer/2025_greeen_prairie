#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GREEEN prairie experiment
# Prepare data ####
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2025-06-19



### Packages ###
library(renv)
library(installr)
library(here)
library(tidyverse)
library(readxl)
library(TNRS)
library(GIFT)
library(FD)

### Start ###
# Create hashtag infront of a line: shift + strg + c
rm(list = ls())
# installr::updateR(
#   browse_news = FALSE,
#   install_R = TRUE,
#   copy_packages = TRUE,
#   copy_site_files = TRUE,
#   keep_old_packages = FALSE,
#   update_packages = FALSE,
#   start_new_R = FALSE,
#   quit_R = TRUE
#   )



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Load data #################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## 1 Sites ####################################################################


treatment_1 <- read_xlsx(
  here("data", "raw", "data_raw_treatments.xlsx"), sheet = "SWMREC",
  col_names = TRUE
) %>%
  mutate(id_plot = str_c("SWS_", id_plot))

treatment_2 <- read_xlsx(
  here("data", "raw", "data_raw_treatments.xlsx"), sheet = "Lux Arbor",
  col_names = TRUE
) %>%
  mutate(id_plot = str_c("Lux_", id_plot))

treatment_3 <- read_xlsx(
  here("data", "raw", "data_raw_treatments.xlsx"), sheet = "NW Station",
  col_names = TRUE
) %>%
  mutate(id_plot = str_c("NWS_", id_plot))

soil_1 <- read_xlsx(
  here("data", "raw", "data_raw_soil.xlsx"), sheet = "Lux"
)

soil_2 <- read_xlsx(
  here("data", "raw", "data_raw_soil.xlsx"), sheet = "SW Station"
)

soil_3 <- read_xlsx(
  here("data", "raw", "data_raw_soil.xlsx"), sheet = "NW Station"
)

excel_sheets(here("data", "raw", "data_raw_flowers_2017.xlsx"))

flowers <- read_xlsx(
  here("data", "raw", "data_raw_flowers_2017.xlsx"), "Floral data"
)


## 2 Species ##################################################################


excel_sheets(here("data", "raw", "data_raw_species.xlsx"))

species <- read_xlsx(
  here("data", "raw", "data_raw_species.xlsx"),
  col_names = TRUE, na = c("", "NA", "na"), col_types =
    cols(
      .default = "?"
    )
  )

species_seeded <- read_xlsx(
  here("data", "raw", "data_raw_species_seeded.xlsx"), sheet = "To Mix"
)



## 4 Combine datasets #########################################################


soil <- soil_1 %>%
  bind_rows(soil_2, soil_3)

treatment <- treatment_1 %>%
  bind_rows(treatment_2, treatment_3)

sites <- treatment %>%
  full_join(soil, by = "id_plot")

species <- species %>%
  full_join(species_seeded, by = "name")

rm(list = setdiff(ls(), c("species", "sites", "flowers")))



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Create variables ###########################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## 1 Names from TNRS database #################################################


metadata <- TNRS_metadata()
metadata$version
metadata$sources %>% tibble()


### a Harmonize names of traits matrix ----------------------------------

traits <- species %>%
  select(name)

## Run only once and than load saved file
harmonized_names <- traits %>%
    rowid_to_column("id") %>%
    select(id, name) %>%
    TNRS::TNRS(
      sources = c("wcvp", "wfo"), # first use WCVP and alternatively WFO
      classification = "wfo", # family classification
      mode = "resolve"
    )

write_csv(
    harmonized_names, here("data", "processed", "data_processed_tnrs.csv")
    )

names_traits <- read.csv(
  here("data", "processed", "data_processed_tnrs_traits.csv")
  ) %>%
  rename_with(tolower)


### b Summarize duplicates of traits matrix ------------------------------------

data <- traits %>%
  rename("name_submitted" = "name") %>%
  left_join(
    names_traits %>% select(name_submitted, name_matched, accepted_name),
    by = "name_submitted"
    ) %>%
  select(name_submitted, name_matched, everything())

# --> No vegetation surveys yet
  
# data <- traits %>%
#   right_join(
#     species %>% select(accepted_name), by = c("Name_matched" = "accepted_name")
#     ) %>%
#   rename(accepted_name = Name_matched) %>%
#   left_join(data_names, by = "accepted_name") %>%
#   select(name_submitted, accepted_name, everything())

data %>% filter(duplicated(accepted_name))

data_summarized <- data %>%
  group_by(accepted_name) %>%
  summarize(across(everything(), ~ first(.x))) %>%
  select(name_submitted, accepted_name, everything())

data_summarized %>% filter(duplicated(accepted_name))

traits <- data_summarized %>%
  left_join(
    names_traits %>%
      select(
        name_submitted, taxonomic_status, accepted_family,
        accepted_name_url
        ),
    by = "name_submitted"
    ) %>%
  select(
    accepted_name, taxonomic_status, accepted_family, everything(),
    accepted_name_url
    )

rm(list = setdiff(ls(), c("species", "sites", "traits")))


### c Summarize duplicates of species matrix -----------------------------------

# --> no vegetation surveys yet

# data <- species %>% 
#   rename(name_submitted = name) %>%
#   left_join(
#     data_names %>% select(name_submitted, accepted_name),
#     by = "name_submitted"
#   ) %>%
#   select(name_submitted, accepted_name, everything())
# 
# data %>% filter(duplicated(accepted_name))
# 
# data_summarized <- data %>%
#   group_by(accepted_name) %>%
#   summarize(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))
# 
# data_summarized %>% filter(duplicated(accepted_name))
# 
# species <- data_summarized



## 4 Traits from GIFT database ################################################


### a Load traits from GIFT ---------------------------------------------------

trait_ids <- c("1.2.2", "1.6.3", "3.2.3", "4.1.3")

GIFT::GIFT_traits_meta() %>%
  filter(Lvl3 %in% trait_ids) %>%
  tibble()

data_gift <- GIFT::GIFT_traits(
  trait_IDs = trait_ids,
  agreement = 0.66, bias_ref = FALSE, bias_deriv = FALSE
)


### b Combine gift and traits -------------------------------------------------

data <- traits %>%
  left_join(
    gift %>%
      select(
        accepted_name, trait_value_1.6.3, trait_value_3.2.3, trait_value_4.1.3,
        trait_value_1.2.2
        ) %>%
      mutate(
        accepted_name = str_replace(
          accepted_name, "Avenula pubescens", "Helictotrichon pubescens"
          ),
        accepted_name = str_replace(
          accepted_name, "Betonica officinalis", "Stachys officinalis"
          ),
        accepted_name = str_replace(
          accepted_name,
          "Cerastium fontanum", "Cerastium fontanum subsp. vulgare"
          ),
        accepted_name = str_replace(
          accepted_name,
          "Festuca pratensis", "Lolium pratense"
          ),
        accepted_name = str_replace(
          accepted_name, "Potentilla verna", "Potentilla tabernaemontani"
          ),
        accepted_name = str_replace(
          accepted_name, "Tragopogon orientalis", "Tragopogon pratensis"
          )
      ),
    by = "accepted_name"
  ) %>%
  rename(
    family = accepted_family,
    sla = trait_value_4.1.3,
    height = trait_value_1.6.3,
    seedmass = trait_value_3.2.3,
    lifeform = trait_value_1.2.2
  )

data %>% filter(duplicated(accepted_name))

data_summarized <- data %>%
  group_by(accepted_name) %>%
  summarize(across(everything(), ~ first(.x, na_rm = TRUE))) %>%
  select(accepted_name, sla, height, seedmass, lifeform, everything())

data_summarized  %>%
  filter(duplicated(accepted_name))

gift %>%
  filter(str_detect(accepted_name, "Lolium pratense")) %>%
  select(1:2, starts_with("trait"))

traits <- data_summarized %>%
  mutate(
    across(c("R1A", "R22", "both"), replace_na, 0),
    sla = if_else(accepted_name == "Festuca nigrescens", 193.57, sla), # Value of Festuca rubra (GIFT traitbase)
    sla = if_else(accepted_name == "Lolium pratense", 214.00, sla), # Value of LEDA traitbase
    sla = if_else(accepted_name == "Festuca rupicola", 193.57, sla), # Value of Festuca rubra (GIFT traitbase)
    sla = if_else(accepted_name == "Medicago falcata", 219.5, sla), # Value of Medicago sativa (GIFT traitbase)
    height = if_else(accepted_name == "Lolium pratense", 0.55, height), # Value of LEDA traitbase
    height = if_else(accepted_name == "Medicago falcata", 0.52, height), # Value of Medicago sativa (GIFT traitbase)
    seedmass = if_else(
      accepted_name == "Leucanthemum ircutianum", 0.0008119666, seedmass
      ) # Value of Leucanthemum vulgare
    )


rm(list = setdiff(ls(), c("species", "sites", "traits")))



## 5 Alpha diversity ##########################################################


### a Species richness -------------------------------------------------------

richness <- species %>%
  left_join(traits, by = "name") %>%
  select(
    name, status, redlist_germany, target, starts_with("X")
  ) %>%
  pivot_longer(names_to = "id", values_to = "n", cols = starts_with("X")) %>%
  mutate(n = if_else(n > 0, 1, 0)) %>%
  group_by(id)

#### Total species richness ###
richness_total <- richness %>%
  summarise(species_richness = sum(n, na.rm = TRUE)) %>%
  ungroup()

#### Red list Germany (species richness) ###
richness_rlg <- richness %>%
  filter(rlg == "1" | rlg == "2" | rlg == "3" | rlg == "V") %>%
  summarise(rlg_richness = sum(n, na.rm = TRUE)) %>%
  ungroup()

#### Target species (species richness) ###
richness_target <- richness %>%
  filter(target != "no") %>%
  summarise(target_richness = sum(n, na.rm = TRUE)) %>%
  ungroup()

sites_dikes <- sites_dikes %>%
  right_join(richness_total, by = "id") %>%
  right_join(richness_rlg, by = "id") %>%
  right_join(richness_target, by = "id")
  mutate(
    target_richness_ratio = target_richness / species_richness
  )


### b Species eveness ---------------------------------------------

data <- species_dikes %>%
  mutate(across(where(is.numeric), ~ replace(., is.na(.), 0))) %>%
  pivot_longer(-name, names_to = "id", values_to = "value") %>%
  pivot_wider(names_from = "name", values_from = "value") %>%
  column_to_rownames("id") %>%
  diversity(index = "shannon") %>%
  as_tibble(rownames = NA) %>%
  rownames_to_column(var = "id") %>%
  mutate(id = factor(id)) %>%
  rename(shannon = value)
sites_dikes <- sites_dikes %>%
  left_join(data, by = "id") %>%
  mutate(eveness = shannon / log(species_richness))

rm(list = setdiff(ls(), c("species", "sites", "traits", "coordinates")))



## 6 Calculation of CWMs ######################################################


### a CWM Plant height 1.6.3 --------------------------------------------------

### b CWM Seed mass 3.2.3 -----------------------------------------------------

### c CWM SLA 4.1.3 -----------------------------------------------------------



### d Add to sites table ------------------------------------------------------


rm(list = setdiff(ls(), c("species", "sites", "traits")))



## 7 Finalization ############################################################


### a Rounding ----------------------------------------------------------------


### b Final selection of variables --------------------------------------------

traits <- traits %>%
  select(
    accepted_name, taxonomic_status, family, status, redlist_germany,
    everything(), accepted_name_url
    )



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# C Save processed data #######################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



write_csv(
  sites,
  here("data", "processed", "data_processed_sites.csv")
)
write_csv(
  species,
  here("data", "processed", "data_processed_species.csv")
)
write_csv(
  traits,
  here("data", "processed", "data_processed_traits.csv")
)
