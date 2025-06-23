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
library(writexl)
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


### a Single datasets ---------------------------------------------------------

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

excel_sheets(here("data", "raw", "data_raw_species.xlsx"))

covers_2015 <- read_xlsx(
  here("data", "raw", "data_raw_species.xlsx"), sheet = "1x1 covers Sept 2015",
  na = c("", "NA", "na")
)

covers_2016 <- read_xlsx(
  here("data", "raw", "data_raw_species.xlsx"), sheet = "1x1 covers 2016"
)

covers_2017 <- read_xlsx(
  here("data", "raw", "data_raw_species.xlsx"), sheet = "1x1 covers 2017"
)

covers_2018 <- read_xlsx(
  here("data", "raw", "data_raw_species.xlsx"), sheet = "1x1 covers 2018"
)

covers_2019 <- read_xlsx(
  here("data", "raw", "data_raw_species.xlsx"), sheet = "1x1 covers 2019"
)

excel_sheets(here("data", "raw", "data_raw_flowers_2017.xlsx"))

flowers <- read_xlsx(
  here("data", "raw", "data_raw_flowers_2017.xlsx"), "Floral data"
)


### b Combine data -------------------------------------------------------------

soil <- soil_1 %>%
  bind_rows(soil_2, soil_3)

treatment <- treatment_1 %>%
  bind_rows(treatment_2, treatment_3)

covers <- covers_2015 %>%
  bind_rows(covers_2016, covers_2017, covers_2018, covers_2019) %>%
  mutate(
    id_plot = if_else(
      site == "Lux Arbor", str_c("Lux_", plot), if_else(
        site == "SW Station", str_c("SWS_", plot), if_else(
          site == "NW Station", str_c("NWS_", plot), "warning"
        )))
  ) %>%
  select(
    id_plot, site, date_surveyed, date_entered, botanist, veg, bare, litter, rock,
    moss, moss_lichens, sum, notes
  )

sites <- treatment %>%
  full_join(soil, by = "id_plot")



## 2 Species ##################################################################


### a Single datasets ---------------------------------------------------------

excel_sheets(here("data", "raw", "data_raw_species.xlsx"))

species_seeded <- read_xlsx(
  here("data", "raw", "data_raw_species_seeded.xlsx"), sheet = "To Mix"
) %>%
  pivot_longer(-name, names_to = "species_pool", values_to = "abundance") %>%
  mutate(
    species_pool = str_replace(species_pool, "species_pool_", ""),
    species_pool = as.numeric(species_pool)
    ) %>%
  filter(abundance > 0) %>%
  left_join(
    sites %>% select(id_plot, species_pool), by = "species_pool",
    relationship = "many-to-many"
    ) %>%
  select(-species_pool)

species_2015_1x1 <- read_xlsx(
  here("data", "raw", "data_raw_species.xlsx"), sheet = "1x1 veg Sept 2015",
  na = c("", "NA", "na")
  )

species_2015_5x5 <- read_xlsx(
  here("data", "raw", "data_raw_species.xlsx"), sheet = "5x5 veg Sept 2015"
  )

species_2016_1x1 <- read_xlsx(
  here("data", "raw", "data_raw_species.xlsx"), sheet = "1x1 veg 2016"
)

species_2016_5x5 <- read_xlsx(
  here("data", "raw", "data_raw_species.xlsx"), sheet = "5x5 veg 2016"
)

species_2017_1x1 <- read_xlsx(
  here("data", "raw", "data_raw_species.xlsx"), sheet = "1x1 veg 2017"
)

species_2017_5x5 <- read_xlsx(
  here("data", "raw", "data_raw_species.xlsx"), sheet = "5x5 veg 2017"
)

species_2018_1x1 <- read_xlsx(
  here("data", "raw", "data_raw_species.xlsx"), sheet = "1x1 veg 2018"
)

species_2018_5x5 <- read_xlsx(
  here("data", "raw", "data_raw_species.xlsx"), sheet = "5x5 veg 2018"
)

species_2019_1x1 <- read_xlsx(
  here("data", "raw", "data_raw_species.xlsx"), sheet = "1x1 veg 2019"
)

species_2019_5x5 <- read_xlsx(
  here("data", "raw", "data_raw_species.xlsx"), sheet = "5x5 veg 2019"
)


### b Combine data -------------------------------------------------------------

species_1x1 <- species_2015_1x1 %>%
  bind_rows(
    species_2016_1x1, species_2017_1x1, species_2018_1x1, species_2019_1x1
    ) %>%
  mutate(
    id_plot = if_else(
      site == "Lux Arbor", str_c("Lux_", plot), if_else(
        site == "SW Station", str_c("SWS_", plot), if_else(
          site == "NW Station", str_c("NWS_", plot), "warning"
        ))),
    plot_size = 1
  ) %>%
  rename(name = species, abundance = cover) %>%
  select(
    id_plot, plot_size, date_surveyed, date_entered, botanist,
    species_non_corrected, name, abundance, notes
    )

species_5x5 <- species_2015_5x5 %>%
  bind_rows(
    species_2016_5x5, species_2017_5x5, species_2018_5x5, species_2019_5x5
  ) %>%
  rename(name = species) %>%
  mutate(
    id_plot = if_else(
      site == "Lux Arbor", str_c("Lux_", plot), if_else(
        site == "SW Station", str_c("SWS_", plot), if_else(
          site == "NW Station", str_c("NWS_", plot), "warning"
        ))),
    plot_size = 25
  ) %>%
  select(
    id_plot, plot_size, date_surveyed, date_entered, botanist,
    species_non_corrected, name, notes
  )

species <- species_seeded %>%
  bind_rows(species_1x1, species_5x5) %>%
  mutate(
    date_entered = date(date_entered),
    date_surveyed = date(date_surveyed)
  ) %>%
  left_join(sites %>% select(id_plot, species_pool))


### Name resolving ####

problems <- species_1x1 %>%
  bind_rows(species_5x5) %>%
  mutate(
    date_entered = date(date_entered),
    date_surveyed = date(date_surveyed)
  ) %>%
  filter(
    !(is.na(notes)),
    !(str_detect(name, "^[:upper:]{6}$"))
    )

write_xlsx(
  problems,
  here("data", "processed", "data_processed_species_problems.xlsx")
)


rm(list = setdiff(ls(), c("species", "sites", "flowers", "covers",
                          "problems", "species_1x1", "species_5x5")))



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Create variables ###########################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## 1 Names from TNRS database #################################################


metadata <- TNRS_metadata()
metadata$version
metadata$sources %>% tibble()


### a Replace abbreviations ----------------------------------------------------

traits <- species %>%
  select(name) %>%
  unique() %>%
  filter(str_detect(name, "^[:upper:]{6}$"))

write_xlsx(
  traits,
  here("data", "processed", "data_processed_traits.xlsx")
)

  mutate(
    name = str_replace(name, "ACENEG", ""),
    name = str_replace(name, "ACERUB", "Acer rubra"),
    name = str_replace(name, "ACESPA", ""),
    name = str_replace(name, "ACHMIL", "Achillea millefolium"),
    name = str_replace(name, "AGRGIG", "Agrostis gigantea"),
    name = str_replace(name, "AMBART", "Ambrosia artimisifolia"),
    name = str_replace(name, "ALLPET", "Alliaria petiolata"),
    name = str_replace(name, "ALLSPP", ""),
    name = str_replace(name, "AMARET", ""),
    name = str_replace(name, "AMASPP", ""),
    name = str_replace(name, "AMBART", ""),
    name = str_replace(name, "ANDGER", ""),
    name = str_replace(name, "ARAGLA", ""),
    name = str_replace(name, "ARATHA", ""),
    name = str_replace(name, "ARGGIG", ""),
    name = str_replace(name, "ASCAMP", ""),
    name = str_replace(name, "ASCSTR", ""),
    name = str_replace(name, "ASCSYR", "Asclepias syriaca"),
    name = str_replace(name, "ASCTUB", "Asclepias tuberosa"),
    name = str_replace(name, "ASPOFF", ""),
    name = str_replace(name, "ASTCAN", ""),
    name = str_replace(name, "ASTERI", "Aster ericoides"),
    name = str_replace(name, "ASTLAE", "Aster laevis"),
    name = str_replace(name, "ASTPIL", ""),
    name = str_replace(name, "AUTGRA", ""),
    name = str_replace(name, "BAPLAC", "Baptisia lactaea"),
    name = str_replace(name, "BARVUL", ""),
    name = str_replace(name, "BERINC", ""),
    name = str_replace(name, "BERVUL", ""),
    name = str_replace(name, "BOUCUR", ""),
    name = str_replace(name, "BORINE", ""),
    name = str_replace(name, "BOUCUR", ""),
    name = str_replace(name, "BRANAP", "Brassica napus"),
    name = str_replace(name, "BRASPP", "Brassica sp."),
    name = str_replace(name, "BROINE", ""),
    name = str_replace(name, "BROJAP", ""),
    name = str_replace(name, "BROKAL", ""),
    name = str_replace(name, "BROTEC", ""),
    name = str_replace(name, "CARDAV", ""),
    name = str_replace(name, "CENSTO", "Centaurea stoebe"),
    name = str_replace(name, "CENSTR", ""),
    name = str_replace(name, "CENTRO", ""),
    name = str_replace(name, "CHAFAS", "Chamaecrista fasciculata"),
    name = str_replace(name, "CHAMAC", ""),
    name = str_replace(name, "CHEALB", "Chenopodium album"),
    name = str_replace(name, "CHOJUN", ""),
    name = str_replace(name, "CIRCAN", "Circaea canadensis"),
    name = str_replace(name, "CIRSPP", "Circaea sp."),
    name = str_replace(name, "CONCAN", ""),
    name = str_replace(name, "CORLAN", "Coreopsis lanceolata"),
    name = str_replace(name, "CYPFIL", ""),
    name = str_replace(name, "DACCAR", ""),
    name = str_replace(name, "DACGLO", "Dactylis glomerata"),
    name = str_replace(name, "DACGLU", ""),
    name = str_replace(name, "DALPIN", ""),
    name = str_replace(name, "DALPUR", "Dalea purpurea"),
    name = str_replace(name, "DANSPI", ""),
    name = str_replace(name, "DAUCAR", "Daucus carota"),
    name = str_replace(name, "DAUPUR", ""),
    name = str_replace(name, "DESCAN", ""),
    name = str_replace(name, "DESSPP", "Desmodium sp."),
    name = str_replace(name, "DICSPP", ""),
    name = str_replace(name, "DIGCOG", ""),
    name = str_replace(name, "DIGHIS", ""),
    name = str_replace(name, "DIGISC", ""),
    name = str_replace(name, "DIGISH", ""),
    name = str_replace(name, "DIGSAN", ""),
    name = str_replace(name, "DIGSIC", ""),
    name = str_replace(name, "DIGSPP", ""),
    name = str_replace(name, "DRYARG", ""),
    name = str_replace(name, "ECHPAL", ""),
    name = str_replace(name, "ECHPUR", "Echinacea purpurea"),
    name = str_replace(name, "ELAUMB", ""),
    name = str_replace(name, "ELEREP", ""),
    name = str_replace(name, "ELYCAN", ""),
    name = str_replace(name, "ELYREP", "Elymus repens"),
    name = str_replace(name, "ELYRER", ""),
    name = str_replace(name, "ELYSPA", ""),
    name = str_replace(name, "ELYSPP", ""),
    name = str_replace(name, "ELYVIR", ""),
    name = str_replace(name, "ERACIL", ""),
    name = str_replace(name, "ERASPE", ""),
    name = str_replace(name, "ERIHIE", ""),
    name = str_replace(name, "ERISPA", ""),
    name = str_replace(name, "ERISPP", ""),
    name = str_replace(name, "ERYYUC", "Eryngium yuccifolium"),
    name = str_replace(name, "EUPGRA", ""),
    name = str_replace(name, "EUTGRA", ""),
    name = str_replace(name, "FALCON", ""),
    name = str_replace(name, "FESRUB", "Festuca rubra"),
    name = str_replace(name, "GENSTO", ""),
    name = str_replace(name, "GNAOBT", ""),
    name = str_replace(name, "GNAPUR", ""),
    name = str_replace(name, "GNAULI", ""),
    name = str_replace(name, "HACVIR", ""),
    name = str_replace(name, "HELOCC", "Helianthemum occidentale"),
    name = str_replace(name, "HIESPP", "Hieracium sp."),
    name = str_replace(name, "HYIPER", ""),
    name = str_replace(name, "HYPPER", ""),
    name = str_replace(name, "HYPRAD", ""),
    name = str_replace(name, "HYPSPP", ""),
    name = str_replace(name, "JUNEFF", "Juncus effusus"),
    name = str_replace(name, "JUNSPP", "Juncus sp."),
    name = str_replace(name, "JUNTEN", "Juncus tenuis"),
    name = str_replace(name, "KOEMAC", "Koeleria macrantha"),
    name = str_replace(name, "KRISPP", ""),
    name = str_replace(name, "LACCAN", ""),
    name = str_replace(name, "LESCAP", "Lespedeza capitata"),
    name = str_replace(name, "LESHIR", "Lespedeza hirta"),
    name = str_replace(name, "LESVIR", "Lespedeza virginica"),
    name = str_replace(name, "LUPDIF", ""),
    name = str_replace(name, "MALNEG", ""),
    name = str_replace(name, "MALSPP", ""),
    name = str_replace(name, "MEDLUP", "Medicago lupulina"),
    name = str_replace(name, "MEDSAT", "Medicago sativa"),
    name = str_replace(name, "MELALB", "Melilotus albus"),
    name = str_replace(name, "MELLUP", ""),
    name = str_replace(name, "MELOFF", "Melilotus officinalis"),
    name = str_replace(name, "MELSPP", "Melilotus sp."),
    name = str_replace(name, "MOLVER", ""),
    name = str_replace(name, "MONFIS", "Monarda fistulosa"),
    name = str_replace(name, "MONPUN", "Monarda punctata"),
    name = str_replace(name, "MORALB", ""),
    name = str_replace(name, "MORLAB", ""),
    name = str_replace(name, "MORRUB", ""),
    name = str_replace(name, "MORSPP", ""),
    name = str_replace(name, "MULVIR", ""),
    name = str_replace(name, "OENBIE", "Oenothera biennis"),
    name = str_replace(name, "OENSPP", "Oenothera sp."),
    name = str_replace(name, "OLIRIG", ""),
    name = str_replace(name, "OXALIS", ""),
    name = str_replace(name, "OXASPP", "Oxalis sp."),
    name = str_replace(name, "OXASTR", "Oxalis stricta"),
    name = str_replace(name, "PANCAN", ""),
    name = str_replace(name, "PANCAP", ""),
    name = str_replace(name, "PANCGO", ""),
    name = str_replace(name, "PANDIC", ""),
    name = str_replace(name, "PANSPP", "Panicum sp."),
    name = str_replace(name, "PANVIR", "Panicum virgatum"),
    name = str_replace(name, "PASSET", ""),
    name = str_replace(name, "PASSPP", ""),
    name = str_replace(name, "PEDDIG", ""),
    name = str_replace(name, "PENDIG", "Penstemon digitalis"),
    name = str_replace(name, "PENHIR", "Penstemon hirsutus"),
    name = str_replace(name, "PHLPRA", ""),
    name = str_replace(name, "PHYAME", ""),
    name = str_replace(name, "PHYHET", ""),
    name = str_replace(name, "PLALAN", "Plantago lanceolata"),
    name = str_replace(name, "PLAMAJ", "Plantago major"),
    name = str_replace(name, "POAANN", "Poa annua"),
    name = str_replace(name, "POACOM", ""),
    name = str_replace(name, "POAPRA", "Poa pratensis"),
    name = str_replace(name, "POAPRE", ""),
    name = str_replace(name, "POLCAN", ""),
    name = str_replace(name, "POLCON", ""),
    name = str_replace(name, "POLSPP", ""),
    name = str_replace(name, "POTAGR", ""),
    name = str_replace(name, "POTARG", "Potentilla arguta"),
    name = str_replace(name, "POTSIM", ""),
    name = str_replace(name, "PRUNUS", ""),
    name = str_replace(name, "PRUSER", "Prunus serotina"),
    name = str_replace(name, "PRUSPP", "Prunus sp."),
    name = str_replace(name, "PSEOBT", ""),
    name = str_replace(name, "QUESPP", "Quercus sp."),
    name = str_replace(name, "QUEVEL", ""),
    name = str_replace(name, "RATPIN", "Ratibida pinnata"),
    name = str_replace(name, "RHUSPA", ""),
    name = str_replace(name, "RHUSPP", "Rhus sp."),
    name = str_replace(name, "RHUTYP", "Rhus typhina"),
    name = str_replace(name, "RUBALL", ""),
    name = str_replace(name, "RUBFLA", ""),
    name = str_replace(name, "RUBOCC", ""),
    name = str_replace(name, "RUBRLA", ""),
    name = str_replace(name, "RUDHIR", "Rudbeckia hirta"),
    name = str_replace(name, "RUMACE", "Rumex acetosa"),
    name = str_replace(name, "RUMACL", ""),
    name = str_replace(name, "RUMCRI", "Rumex crispus"),
    name = str_replace(name, "RUMOBT", "Rumex obtusifolius"),
    name = str_replace(name, "SCHSCO", ""),
    name = str_replace(name, "SCRLAN", ""),
    name = str_replace(name, "SCRNUT", ""),
    name = str_replace(name, "SETITA", ""),
    name = str_replace(name, "SETMEP", ""),
    name = str_replace(name, "SETSPP", "Setaria sp."),
    name = str_replace(name, "SETVIR", ""),
    name = str_replace(name, "SILLAT", ""),
    name = str_replace(name, "SILSPP", "Silphyum sp."),
    name = str_replace(name, "SILVUL", "Silene vulgaris"),
    name = str_replace(name, "SINVUL", ""),
    name = str_replace(name, "SOLALT", ""),
    name = str_replace(name, "SOLCAN", "Solidago canadensis"),
    name = str_replace(name, "SOLCAR", ""),
    name = str_replace(name, "SOLNEM", "Solidago nemoralis"),
    name = str_replace(name, "SOLNUT", ""),
    name = str_replace(name, "SOLPTY", ""),
    name = str_replace(name, "SOLRIG", "Solidago rigida"),
    name = str_replace(name, "SOLSPE", "Solidago speciosa"),
    name = str_replace(name, "SORNUT", ""),
    name = str_replace(name, "SPOCLA", ""),
    name = str_replace(name, "SPOSPP", ""),
    name = str_replace(name, "STEMED", ""),
    name = str_replace(name, "STEMEP", ""),
    name = str_replace(name, "STESPP", ""),
    name = str_replace(name, "SYMERI", "Symphiotrichum ericoides"),
    name = str_replace(name, "SYMLAE", ""),
    name = str_replace(name, "SYMLAN", ""),
    name = str_replace(name, "SYMPIL", ""),
    name = str_replace(name, "SYMSPP", "Symphiotrichum sp."),
    name = str_replace(name, "TAROFF", "Taraxacum officinale"),
    name = str_replace(name, "TEUCAN", ""),
    name = str_replace(name, "TOXRAD", ""),
    name = str_replace(name, "TRADUB", "Tragopogon dubium"),
    name = str_replace(name, "TRIARV", "Trifolium arvense"),
    name = str_replace(name, "TRICAM", ""),
    name = str_replace(name, "TRIFLA", ""),
    name = str_replace(name, "TRIPRA", "Trifolium pratense"),
    name = str_replace(name, "TRIREP", "Trifolium repens"),
    name = str_replace(name, "TRISPP", "Trifolium sp."),
    name = str_replace(name, "TURGLA", ""),
    name = str_replace(name, "ULMSPP", ""),
    name = str_replace(name, "URTDIO", "Urtica dioica"),
    name = str_replace(name, "VERSPP", ""),
    name = str_replace(name, "VERSTR", "Verbena stricta"),
    name = str_replace(name, "VERTHA", ""),
    name = str_replace(name, "VERURT", ""),
    name = str_replace(name, "VICSPP", "Vicia sp."),
    name = str_replace(name, "VICVIL", ""),
    name = str_replace(name, "VICCIL", ""),
    name = str_replace(name, "VITAES", ""),
    name = str_replace(name, "VITRIP", ""),
    name = str_replace(name, "VITSPP", ""),
    name = str_replace(name, "ZIZAPT", "Zizea aptera"),
    name = str_replace(name, "ZIZAUR", "")
  )


### b Harmonize names of species matrix ---------------------------------------

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



## 2 Traits from GIFT database ################################################


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



## 3 Alpha diversity ##########################################################


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



## 4 Calculation of CWMs ######################################################


### a CWM Plant height 1.6.3 --------------------------------------------------

### b CWM Seed mass 3.2.3 -----------------------------------------------------

### c CWM SLA 4.1.3 -----------------------------------------------------------



### d Add to sites table ------------------------------------------------------


rm(list = setdiff(ls(), c("species", "sites", "traits")))



## 5 Finalization ############################################################


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
