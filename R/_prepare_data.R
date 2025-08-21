#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GREEEN prairie experiment
# Prepare data ####
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2025-07-18



### Packages ###
library(renv)
library(installr)
library(here)
library(tidyverse)
library(readxl)
library(writexl)
library(TNRS)
library(GIFT)
library(rtry)
library(FD)

### Start ###
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
  here("data", "raw", "data_raw_soil.xlsx"), sheet = "NW Station", na = c("NA")
)

covers <- read_csv(
  here("data", "raw", "data_raw_covers.csv"), na = c("", "NA", "na")
)

excel_sheets(here("data", "raw", "data_raw_flowers.xlsx"))

flowers <- read_xlsx(
  here("data", "raw", "data_raw_flowers.xlsx"), "Floral data"
)


### b Combine data -------------------------------------------------------------

soil <- soil_1 %>%
  bind_rows(soil_2, soil_3) %>%
  select(-year) %>%
  select(id_plot, id_tin, water_cap)

treatment <- treatment_1 %>%
  bind_rows(treatment_2, treatment_3)

covers <- covers %>%
  mutate(
    id_plot = if_else(
      site == "Lux Arbor", str_c("Lux_", plot), if_else(
        site == "SW Station", str_c("SWS_", plot), if_else(
          site == "NW Station", str_c("NWS_", plot), "warning"
        )))
  ) %>%
  select(
    id_plot, site, date_surveyed, date_entered, botanist, veg, bare, litter, rock,
    moss, notes
  )

sites <- treatment %>%
  full_join(soil, by = "id_plot") %>%
  mutate(
    site = if_else(str_detect(id_plot, "SWS_"), "SW Station", if_else(
      str_detect(id_plot, "NWS_"), "NW Station", if_else(
        str_detect(id_plot, "Lux_"), "Lux Arbor", "warning"
    )))
    ) %>%
  relocate(site, .after = id_plot)

rm(list = setdiff(ls(), c("sites", "flowers", "covers")))



## 2 Species ##################################################################


### a Single datasets ---------------------------------------------------------

excel_sheets(here("data", "raw", "data_raw_species_seeded.xlsx"))

species_seeded <- read_xlsx(
  here("data", "raw", "data_raw_species_seeded.xlsx"), sheet = "To Mix"
) %>%
  pivot_longer(-name, names_to = "seeded_pool", values_to = "abundance") %>%
  mutate(
    seeded_pool = str_replace(seeded_pool, "seeded_pool_", ""),
    seeded_pool = as.numeric(seeded_pool)
    ) %>%
  filter(abundance > 0) %>%
  left_join(
    sites %>% select(id_plot, seeded_pool), by = "seeded_pool",
    relationship = "many-to-many"
    ) %>%
  select(-seeded_pool) %>%
  mutate(
    date_surveyed = date("2014-09-01"),
    plot_size = "seeded",
    seeded = 1
    )

species <- read_csv(
  here("data", "raw", "data_raw_species.csv"), na = c("", "NA", "na"),
  col_types =
    cols(
      .default = "?",
      date_surveyed = col_date(format = "%Y-%m-%d"),
      plot_size = "c"
    )
  )


### b Combine data -------------------------------------------------------------

data <- species %>%
  mutate(
    id_plot = if_else(
      site == "Lux Arbor", str_c("Lux_", plot), if_else(
        site == "SW Station", str_c("SWS_", plot), if_else(
          site == "NW Station", str_c("NWS_", plot), str_c("warning_", plot)
        )))
  ) %>%
  rename(name = species, abundance = cover) %>%
  select(
    id_plot, site, plot_size, date_surveyed, date_entered, botanist,
    species_original, name, abundance, notes
    )

species <- species_seeded %>%
  bind_rows(data) %>%
  mutate(
    date_entered = date(date_entered),
    date_surveyed = date(date_surveyed),
    date_surveyed = date(date_surveyed),
    year = year(date_surveyed),
    id_plot_year = str_c(id_plot, year, sep = "_")
  ) %>%
  left_join(sites %>% select(id_plot, seeded_pool), by = "id_plot") %>%
  select(
    id_plot_year, id_plot, plot_size, seeded_pool, date_surveyed, date_entered,
    botanist, species_original, name, abundance, seeded, notes
    ) %>%
  arrange(id_plot, date_surveyed, name)


### Name resolving ####

data_check_problems <- species %>%
  mutate(
    date_entered = date(date_entered),
    date_surveyed = date(date_surveyed)
  ) %>%
  mutate(
    problems = if_else(
      is.na(notes) & str_detect(name, "^[:upper:]{6}$"), 0, if_else(
        plot_size == "seeded", 0, if_else(
          name %in% c(
            "DIGISCSAN", "ERASPESPP", "FESRUBAGG", "POTARGE", "POTARGU",
            "Poaceae", "Asteraceae", "Brassicaceae", "Fabaceae",
            "Cyperaceae"
            ), 0, if_else(
              str_detect(notes, "^sample$") &
                str_detect(name, "^[:upper:]{6}$"),
              0, if_else(
                str_detect(notes, "^photo$") &
                  str_detect(name, "^[:upper:]{6}$"),
                0, if_else(str_detect(name, "unknown"), 1, 2)
                )
              )
        )
      )
      ),
    name = if_else(str_detect(name, "^[:upper:]{6}$"), name, tolower(name))
      ) %>%
  arrange(
    desc(problems), name, species_original, id_plot, date_surveyed,
    seeded_pool
    ) %>%
  select(-date_entered, -seeded_pool) %>%
  filter(problems > 0)

rm(list = setdiff(ls(), c("species", "sites", "flowers", "covers")))



## 3 Traits ###################################################################


### a Zirbel et al. (2017) -----------------------------------------------------

# Zirbel CR, Bassett T, Grman E and Brudvig LA (2017) Journal of Applied Ecology https://doi.org/10.1111/1365-2664.12885
# Zirbel CR, Bassett T, Grman E and Brudvig LA (2017) Dryad https://doi.org/10.5061/dryad.2175q

data_2017 <- read_csv(
  here("data", "raw", "data_raw_traits_zirbel_etal_2017.csv"),
  na = c("", "NA", "na")
  ) %>%
  rename(
    name = species, height_2017 = plant.height, sla_2017 = SLA,
    seedmass_2017 = seed.mass
    ) %>%
  mutate(name = str_replace_all(name, "\\.", " ")) %>%
  select(-kew.seed) %>%
  group_by(name) %>%
  summarize(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .)))


### b Zirbel & Brudvig (2020) --------------------------------------------------

# Zirbel, CR, Brudvig LA (2020) Ecology https://doi.org/10.1002/ecy.2971
# Zirbel CR, Brudvig LA (2019) Dryad https://doi.org/10.5061/dryad.bnzs7h46q

data_leaf_area <- read_csv(
  here("data", "raw", "data_raw_traits_zirbel_brudvig_2020_leaf_area.csv"),
  na = c("", "NA", "na")
) %>%
  rename(name = species, leaf_area_2020 = total.leaf.area) %>%
  unite("id", name, number, source, sep = "_") %>%
  mutate(id = str_replace(id, "HEHEL__6_NA", "HELHEL_6_NA")) %>%
  select(id, leaf_area_2020)
data_leaf_mass <- read_csv(
  here("data", "raw", "data_raw_traits_zirbel_brudvig_2020_leaf_mass.csv"),
  na = c("", "NA", "na")
) %>%
  rename(name = species, leaf_mass_2020 = leaf_mass) %>%
  unite("id", name, number, source, sep = "_") %>%
  select(id, leaf_mass_2020)
data_sla <- data_leaf_area %>%
  left_join(data_leaf_mass, by = "id") %>%
  mutate(sla_2020 = leaf_area_2020 / leaf_mass_2020) %>%
  separate(id, into = c("name", "number", "source"), sep = "_") %>%
  select(-number, -source, -starts_with("leaf_")) %>%
  group_by(name) %>%
  summarize(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .)))
data_seed_mass <- read_csv(
  here("data", "raw", "data_raw_traits_zirbel_brudvig_2020_seed_mass.csv"),
  na = c("", "NA", "na")
) %>%
  filter(!(is.na(species))) %>% 
  rename(name = species, seedmass_2020 = seed_mass) %>%
  select(name, seedmass_2020) %>%
  group_by(name) %>%
  summarize(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .)))


### c Combine data -------------------------------------------------------------

traits_zirbel <- data_2017 %>%
  full_join(data_sla, by = "name") %>%
  full_join(data_seed_mass, by = "name") %>%
  mutate(
    name = str_replace(name, "ANDGER", "Andropogon gerardii"),
    name = str_replace(name, "ASCSYR", "Asclepias syriaca"),
    name = str_replace(name, "ASCTUB", "Asclepias tuberosa"),
    name = str_replace(name, "ASTERI", "Aster ericoides"),
    name = str_replace(name, "ASTLAE", "Aster laevis"),
    name = str_replace(name, "ASTOOL", "Aster oolentangiensis"),
    name = str_replace(name, "ASTPIL", "Aster pilosus"),
    name = str_replace(name, "ASTSAG", "Aster sagittifolius"),
    name = str_replace(name, "BAPLAC", "Baptisia alba"),
    name = str_replace(name, "BOUCUR", "Bouteloua curtipendula"),
    name = str_replace(name, "CASFAS", "Cassia fasciculata"),
    name = str_replace(name, "CHAFAS", "Chamaecrista fasciculata"),
    name = str_replace(name, "CORLAN", "Coreopsis lanceolata"),
    name = str_replace(name, "DALPUR", "Dalea purpurea"),
    name = str_replace(name, "DESILL", "Desmodium illinoense"),
    name = str_replace(name, "DRYARG", "Drymocallis arguta"),
    name = str_replace(name, "POTARGUTA", "Drymocallis arguta"),
    name = str_replace(name, "ECHPUR", "Echinacea purpurea"),
    name = str_replace(name, "ERYYUC", "Eryngium yuccifolium"),
    name = str_replace(name, "HELOCC", "Helianthus occidentalis"),
    name = str_replace(name, "LESCAP", "Lespedeza capitata"),
    name = str_replace(name, "LESVIR", "Lespedeza virginica"),
    name = str_replace(name, "LIACYL", "Liatris cylindracea"),
    name = str_replace(name, "LUPPER", "Lupinus perennis"),
    name = str_replace(name, "MONFIS", "Monarda fistulosa"),
    name = str_replace(name, "MONPUN", "Monarda punctata"),
    name = str_replace(name, "PANVIR", "Panicum virgatum"),
    name = str_replace(name, "PENDIG", "Penstemon digitalis"),
    name = str_replace(name, "PENHIR", "Penstemon hirsutus"),
    name = str_replace(name, "PETPUR", "Petalostemom purpureus"),
    name = str_replace(name, "POTARGE", "Potentilla argentea"),
    name = str_replace(name, "POTARG", "Potentilla argentea"),
    name = str_replace(name, "POTARGuta", "Drymocallis arguta"),
    name = str_replace(name, "Potentilla arguta", "Drymocallis arguta"),
    name = str_replace(name, "RATPIN", "Ratibida pinnata"),
    name = str_replace(name, "RUDHIR", "Rudbeckia hirta"),
    name = str_replace(name, "SCHSCO", "Schizachyrium scoparium"),
    name = str_replace(name, "SILTER", "Silphium terebinthinaceum"),
    name = str_replace(name, "SOLALT", "Solidago altissima"),
    name = str_replace(name, "SOLCAN", "Solidago canadensis"),
    name = str_replace(name, "SOLJUN", "Solidago juncea"),
    name = str_replace(name, "SOLNEM", "Solidago nemoralis"),
    name = str_replace(name, "SOLRIG", "Solidago rigida"),
    name = str_replace(name, "SOLSPE", "Solidago speciosa"),
    name = str_replace(name, "SORNUT", "Sorghastrum nutans"),
    name = str_replace(name, "SPOCRY", "Sporobolus cryptandrus"),
    name = str_replace(name, "SYMERI", "Symphyotrichum ericoides"),
    name = str_replace(name, "SYMLAE", "Symphyotrichum laeve"),
    name = str_replace(name, "SYMLAN", "Symphyotrichum lanceolatum"),
    name = str_replace(name, "SYMPIL", "Symphyotrichum pilosum"),
    name = str_replace(name, "SYMUND", "Symphyotrichum undulatum"),
    name = str_replace(name, "TRAOHI", "Tradescantia ohiensis"),
    name = str_replace(name, "VERSTR", "Verbena stricta"),
    name = str_replace(name, "ZIZAPT", "Zizia aptera")
  ) %>%
  mutate(
    sla_2017 = sla_2017 * 10, # transform mm^2 mg^-1 --> cm^2 g^-1
    seedmass_2017 = seedmass_2017 / 1000, # transform mg --> g
    seedmass_2020 = seedmass_2020 / 1000 # transform mg --> g
    ) %>%
  group_by(name) %>%
  summarize(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .)))
  
rm(list = setdiff(
  ls(), c("species", "sites", "flowers", "covers","traits_zirbel"))
  )



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Create variables ###########################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## 1 Names harmonization #######################################################


### a Replace abbreviations ----------------------------------------------------

data_names <- species %>%
  select(name, seeded) %>%
  unique() %>%
  mutate(
    abb = name,
    name = str_replace(name, "ACENEG", "Acer negundo"),
    name = str_replace(name, "ACERUB", "Acer rubrum"),
    name = str_replace(name, "ACESPP", "Acer sp."),
    name = str_replace(name, "ACHMIL", "Achillea millefolium"),
    name = str_replace(name, "AGRGIG", "Agrostis gigantea"),
    name = str_replace(name, "AMASPP", "Amaranthus sp."),
    name = str_replace(name, "AMBART", "Ambrosia artimisiifolia"),
    name = str_replace(name, "ALLPET", "Alliaria petiolata"),
    name = str_replace(name, "ALLSPP", "Allium sp."),
    name = str_replace(name, "ALLVIN", "Allium vineale"),
    name = str_replace(name, "AMASPP", "Amaranthus sp."),
    name = str_replace(name, "AMBART", "Ambrosia artimisiifolia"),
    name = str_replace(name, "ANDGER", "Andropogon gerardii"),
    name = str_replace(name, "APOCAN", "Apocynum cannabinum"),
    name = str_replace(name, "ARAGLA", "Arabis glabra"),
    name = str_replace(name, "ARATHA", "Arabidopsis thaliana"),
    name = str_replace(name, "ASCAMP", "Asclepias amplexicaulis"),
    name = str_replace(name, "ASCSYR", "Asclepias syriaca"),
    name = str_replace(name, "ASCTUB", "Asclepias tuberosa"),
    name = str_replace(name, "ASPOFF", "Asparagus officinalis"),
    name = str_replace(name, "ASTCAN", "Astragalus canadensis"),
    name = str_replace(name, "ASTERI", "Aster ericoides"),
    name = str_replace(name, "ASTLAE", "Aster laevis L."),
    name = str_replace(name, "ASTOOL", "Aster oolentangiensis"),
    name = str_replace(name, "ASTPIL", "Aster pilosus"),
    name = str_replace(name, "ASTSAG", "Aster sagittifolius"),
    name = str_replace(name, "AVESAT", "Avena sativa"),
    name = str_replace(name, "BAPLAC", "Baptisia lactea"),
    name = str_replace(name, "BARVUL", "Barbarea vulgaris"),
    name = str_replace(name, "BERINC", "Berteroa incana"),
    name = str_replace(name, "BERVUL", "Berberis vulgaris"),
    name = str_replace(name, "BETULA", "Betula"),
    name = str_replace(name, "BOUCUR", "Bouteloua curtipendula"),
    name = str_replace(name, "BRANAP", "Brassica napus"),
    name = str_replace(name, "BRASPP", "Brassica sp."),
    name = str_replace(name, "BROINE", "Bromus inermis"),
    name = str_replace(name, "BROJAP", "Bromus japonicus"),
    name = str_replace(name, "BROKAL", "Bromus kalmii"),
    name = str_replace(name, "BROTEC", "Bromus tectorum"),
    name = str_replace(name, "CARSPP", "Carex sp."),
    name = str_replace(name, "CASFAS", "Cassia fasciculata"),
    name = str_replace(name, "CENSTO", "Centaurea stoebe"),
    name = str_replace(name, "CHAFAS", "Chamaecrista fasciculata"),
    name = str_replace(name, "CHAMAC", "Chamaesyce maculata"),
    name = str_replace(name, "CHEALB", "Chenopodium album"),
    name = str_replace(name, "CHOJUN", "Chondrilla juncea"),
    name = str_replace(name, "CIRCAN", "Circaea canadensis"),
    name = str_replace(name, "CIRSPP", "Cirsium sp."),
    name = str_replace(name, "CONCAN", "Conyza canadensis"),
    name = str_replace(name, "CORLAN", "Coreopsis lanceolata"),
    name = str_replace(name, "CYPLUP", "Cyperus lupulinus"),
    name = str_replace(name, "CYPSPP", "Cyperus sp."),
    name = str_replace(name, "DACGLO", "Dactylis glomerata"),
    name = str_replace(name, "DALPUR", "Dalea purpurea"),
    name = str_replace(name, "DANSPI", "Danthonia spicata"),
    name = str_replace(name, "DAUCAR", "Daucus carota"),
    name = str_replace(name, "DESCAN", "Desmodium canadense"),
    name = str_replace(name, "DESILL", "Desmodium illinoense"),
    name = str_replace(name, "DESPAN", "Desmodium paniculatum"),
    name = str_replace(name, "DICSPP", "Dichanthelium sp."),
    name = str_replace(name, "DIGCOG", "Digitaria cognata"),
    name = str_replace(name, "DIGISC", "Digitaria ischaemum"),
    name = str_replace(name, "DIGSAN", "Digitaria sanguinalis"),
    name = str_replace(name, "DIGSPP", "Digitaria sp."),
    name = str_replace(name, "DRYARG", "Drymocallis arguta"),
    name = str_replace(name, "ECHPAL", "Echinacea pallida"),
    name = str_replace(name, "ECHPUR", "Echinacea purpurea"),
    name = str_replace(name, "ELAUMB", "Elaeagnus umbellata"),
    name = str_replace(name, "ELYCAN", "Elymus canadensis"),
    name = str_replace(name, "ELYREP", "Elymus repens"),
    name = str_replace(name, "ELYSPP", "Elymus sp."),
    name = str_replace(name, "ELYVIR", "Elymus virginicus"),
    name = str_replace(name, "ERACIL", "Eragrostis cilianensis"),
    name = str_replace(name, "ERASPESPP", "Eragrostis spectabilis or sp."),
    name = str_replace(name, "EREHIE", "Erechtites hieraciifolius"),
    name = str_replace(name, "ERISPP", "Erigeron sp."),
    name = str_replace(name, "ERYYUC", "Eryngium yuccifolium"),
    name = str_replace(name, "EUPMAC", "Euphorbia maculata"),
    name = str_replace(name, "EUTGRA", "Euthamia graminifolia"),
    name = str_replace(name, "FAGESC", "Fagopyrum esculentum"),
    name = str_replace(name, "FALCON", "Fallopia convolvulus"),
    name = str_replace(name, "FESRUB", "Festuca rubra"),
    name = str_replace(
      name, "FESRUBAGG", "Festuca rubra agg."
      ),
    name = str_replace(name, "FESTRA", "Festuca trachyphylla (Hack.) Hack."),
    name = str_replace(name, "GERSPP", "Geranium sp."),
    name = str_replace(name, "GEUCAN", "Geum canadense"),
    name = str_replace(name, "GLETRI", "Gleditsia triacanthos"),
    name = str_replace(name, "GNAOBT", "Gnaphalium obtusifolium L."),
    name = str_replace(name, "GNAPUR", "Gnaphalium purpureum"),
    name = str_replace(name, "GNASPP", "Gnaphalium sp."),
    name = str_replace(name, "GNAULI", "Gnaphalium uliginosum"),
    name = str_replace(name, "HACVIR", "Hackelia virginiana"),
    name = str_replace(name, "HELOCC", "Helianthus occidentalis"),
    name = str_replace(name, "HIESPP", "Hieracium sp."),
    name = str_replace(name, "HYPPER", "Hypericum perforatum"),
    name = str_replace(name, "HYPRAD", "Hypochaeris radicata"),
    name = str_replace(name, "JUNDIC", "Juncus dichotomus"),
    name = str_replace(name, "JUNEFF", "Juncus effusus"),
    name = str_replace(name, "JUNTEN", "Juncus tenuis"),
    name = str_replace(name, "KOEMAC", "Koeleria macrantha"),
    name = str_replace(name, "KRISPP", "Krigia sp."),
    name = str_replace(name, "LACSPP", "Lactuca sp."),
    name = str_replace(name, "LEPCAM", "Lepidium campestre"),
    name = str_replace(name, "LESCAP", "Lespedeza capitata"),
    name = str_replace(name, "LESHIR", "Lespedeza hirta (L.) Hornem."),
    name = str_replace(name, "LESVIR", "Lespedeza virginica"),
    name = str_replace(name, "LIACYL", "Liatris cylindracea"),
    name = str_replace(name, "LONMAA", "Lonicera maackii"),
    name = str_replace(name, "LUPPER", "Lupinus perennis"),
    name = str_replace(name, "MALNEG", "Malva neglecta"),
    name = str_replace(name, "MALSPP", "Malus sp."),
    name = str_replace(name, "MEDLUP", "Medicago lupulina"),
    name = str_replace(name, "MEDSAT", "Medicago sativa"),
    name = str_replace(name, "MEDSPP", "Medicago sp."),
    name = str_replace(name, "MELALB", "Melilotus albus"),
    name = str_replace(name, "MELOFF", "Melilotus officinalis"),
    name = str_replace(name, "MELSPP", "Melilotus sp."),
    name = str_replace(name, "MOLVER", "Mollugo verticillata"),
    name = str_replace(name, "MONFIS", "Monarda fistulosa"),
    name = str_replace(name, "MONPUN", "Monarda punctata"),
    name = str_replace(name, "MORALB", "Morus alba"),
    name = str_replace(name, "MORRUB", "Morus rubra"),
    name = str_replace(name, "MORSPP", "Morus sp."),
    name = str_replace(name, "OENBIE", "Oenothera biennis"),
    name = str_replace(name, "OENSPP", "Oenothera sp."),
    name = str_replace(name, "OXASPP", "Oxalis sp."),
    name = str_replace(name, "OXASTR", "Oxalis stricta"),
    name = str_replace(name, "PANCAP", "Panicum capillare"),
    name = str_replace(name, "PANDIC", "Panicum dichotomiflorum"),
    name = str_replace(name, "PANSPP", "Panicum sp."),
    name = str_replace(name, "PANVIR", "Panicum virgatum"),
    name = str_replace(name, "PASSET", "Paspalum setaceum"),
    name = str_replace(name, "PASSPP", "Paspalum sp."),
    name = str_replace(name, "PENDIG", "Penstemon digitalis"),
    name = str_replace(name, "PENHIR", "Penstemon hirsutus"),
    name = str_replace(name, "PERMAC", "Persicaria maculosa"),
    name = str_replace(name, "PETPUR", "Petalostemom purpureus"),
    name = str_replace(name, "PHLPRA", "Phleum pratense"),
    name = str_replace(name, "PHYAME", "Phytolacca americana"),
    name = str_replace(name, "PHYHET", "Phytolacca heterotepala"),
    name = str_replace(name, "PHYSALIS", "Physalis"),
    name = str_replace(name, "PHYSPP", "Phytolacca"),
    name = str_replace(name, "PLALAN", "Plantago lanceolata"),
    name = str_replace(name, "PLAMAJ", "Plantago major"),
    name = str_replace(name, "POAANN", "Poa annua"),
    name = str_replace(name, "POACOM", "Poa compressa"),
    name = str_replace(name, "POAPRA", "Poa pratensis"),
    name = str_replace(name, "POASPP", "Poa sp."),
    name = str_replace(name, "POTARGE", "Potentilla argentea"),
    name = str_replace(name, "POTNOR", "Potentilla norvegica"),
    name = str_replace(name, "POTREC", "Potentilla recta"),
    name = str_replace(name, "POTSIM", "Potentilla simplex"),
    name = str_replace(name, "PRUCUL", "Prunus cultivated"),
    name = str_replace(name, "PRUSER", "Prunus serotina"),
    name = str_replace(name, "PRUSPP", "Prunus sp."),
    name = str_replace(name, "PSEOBT", "Pseudognaphalium obtusifolium"),
    name = str_replace(name, "PYRCAL", "Pyrus calleryana"),
    name = str_replace(name, "QUESPP", "Quercus sp."),
    name = str_replace(name, "QUEVEL", "Quercus velutina"),
    name = str_replace(name, "RATPIN", "Ratibida pinnata"),
    name = str_replace(name, "RHUSPP", "Rhus sp."),
    name = str_replace(name, "RHUTYP", "Rhus typhina"),
    name = str_replace(name, "RUBALL", "Rubus allegheniensis"),
    name = str_replace(name, "RUBFLA", "Rubus flagellaris"),
    name = str_replace(name, "RUBOCC", "Rubus occidentalis"),
    name = str_replace(name, "RUDHIR", "Rudbeckia hirta"),
    name = str_replace(name, "RUMACE", "Rumex acetosa"),
    name = str_replace(name, "RUMACL", "Rumex acetosella"),
    name = str_replace(name, "RUMCRI", "Rumex crispus"),
    name = str_replace(name, "RUMOBT", "Rumex obtusifolius"),
    name = str_replace(name, "RUMHAS", "Rumex sp."),
    name = str_replace(name, "SCHSCO", "Schizachyrium scoparium"),
    name = str_replace(name, "SCRLAN", "Scrophularia lanceolata"),
    name = str_replace(name, "SETITA", "Setaria italica"),
    name = str_replace(name, "SETMEP", "Setaria sp."),
    name = str_replace(name, "SETSPP", "Setaria sp."),
    name = str_replace(name, "SETVIR", "Setaria viridis"),
    name = str_replace(name, "SILLAT", "Silene latifolia"),
    name = str_replace(name, "SILSPP", "Silene sp."),
    name = str_replace(name, "SILTER", "Silphium terebinthinaceum"),
    name = str_replace(name, "SILVUL", "Silene vulgaris"),
    name = str_replace(name, "SOLALT", "Solidago altissima"),
    name = str_replace(name, "SOLCAN", "Solidago canadensis"),
    name = str_replace(name, "SOLCAR", "Solanum carolinense"),
    name = str_replace(name, "SOLJUN", "Solidago juncea"),
    name = str_replace(name, "SOLNEM", "Solidago nemoralis"),
    name = str_replace(name, "SOLPTY", "Solanum ptychanthum"),
    name = str_replace(name, "SOLRIG", "Solidago rigida"),
    name = str_replace(name, "SOLSPE", "Solidago speciosa Nutt."),
    name = str_replace(name, "SOLSPP", "Solidago sp."),
    name = str_replace(name, "SORNUT", "Sorghastrum nutans"),
    name = str_replace(name, "SPOCRY", "Sporobolus cryptandrus"),
    name = str_replace(name, "SPOHET", "Sporobolus heterolepis"),
    name = str_replace(name, "STEMED", "Stellaria media"),
    name = str_replace(name, "SYMERI", "Symphyotrichum ericoides"),
    name = str_replace(name, "SYMLAE", "Symphyotrichum laeve (L.) A.Love & D.Love"),
    name = str_replace(name, "SYMLAN", "Symphyotrichum lanceolatum"),
    name = str_replace(name, "SYMPIL", "Symphyotrichum pilosum"),
    name = str_replace(name, "SYMSPP", "Symphyotrichum sp."),
    name = str_replace(name, "SYMUND", "Symphyotrichum undulatum"),
    name = str_replace(name, "TAROFF", "Taraxacum officinale"),
    name = str_replace(name, "TOXRAD", "Toxicodendron radicans"),
    name = str_replace(name, "TRAOHI", "Tradescantia ohiensis"),
    name = str_replace(name, "TRASPP", "Tragopogon sp."),
    name = str_replace(name, "TRIARV", "Trifolium arvense"),
    name = str_replace(name, "TRICAM", "Trifolium campestre"),
    name = str_replace(name, "TRIFLA", "Trifolium flavus"),
    name = str_replace(name, "TRIHYB", "Trifolium hybridum"),
    name = str_replace(name, "TRIPRA", "Trifolium pratense"),
    name = str_replace(name, "TRIREP", "Trifolium repens"),
    name = str_replace(name, "TRISPP", "Trifolium sp."),
    name = str_replace(name, "TURGLA", "Turritis glabra"),
    name = str_replace(name, "ULMPUM", "Ulmus pumila"),
    name = str_replace(name, "ULMSPP", "Ulmus sp."),
    name = str_replace(name, "URTDIO", "Urtica dioica"),
    name = str_replace(name, "VERSPP", "Verbena sp."),
    name = str_replace(name, "VERSTR", "Verbena stricta"),
    name = str_replace(name, "VERTHA", "Verbascum thapsus"),
    name = str_replace(name, "VERURT", "Verbena urticifolia"),
    name = str_replace(name, "VICSPP", "Vicia sp."),
    name = str_replace(name, "VICVIL", "Vicia villosa"),
    name = str_replace(name, "VINSPP", "Vinca sp."),
    name = str_replace(name, "VIOSPP", "Viola sp."),
    name = str_replace(name, "VITAES", "Vitis aestivalis"),
    name = str_replace(name, "VITVIN", "Vitis vinifera"),
    name = str_replace(name, "VITRIP", "Vitis riparia"),
    name = str_replace(name, "VITSPP", "Vitis sp."),
    name = str_replace(name, "ZIZAPT", "Zizia aptera")
  ) %>%
  select(abb, name, seeded) %>%
  group_by(abb, name) %>%
  summarize(seeded = sum(seeded, na.rm = TRUE)) %>%
  ungroup()


### b Harmonize names of species matrix ---------------------------------------

metadata <- TNRS_metadata()
metadata$version
metadata$sources %>% tibble()

data_names_resolved <- data_names %>%
  rowid_to_column("id") %>%
  select(id, name) %>%
  TNRS::TNRS(
    sources = c("wcvp", "wfo"), # first use WCVP and alternatively WFO
    classification = "wfo", # family classification
    mode = "resolve"
  ) %>%
  rename_with(tolower) %>%
  mutate(
    accepted_name = str_replace(
      accepted_name, "Baptisia alba var. macrophylla", "Baptisia alba"
    )
  )

data_check_warnings <- data_names %>%
  rename("name_submitted" = "name") %>%
  left_join(
    data_names_resolved %>%
      select(
        name_submitted, accepted_name, overall_score, warnings,
        warningseng, source
        ),
    by = "name_submitted"
    ) %>%
  arrange(desc(warnings), overall_score)

data_check_duplicated <- data_names %>%
  rename("name_submitted" = "name") %>%
  left_join(
    data_names_resolved %>%
      select(name_submitted, accepted_name, accepted_family, accepted_name_url),
    by = c("name_submitted")
  ) %>%
  select(abb, name_submitted, accepted_name, accepted_family, seeded) %>%
  add_count(accepted_name) %>%
  arrange(desc(n), accepted_name) %>%
  filter(n > 1)

rm(list = setdiff(
  ls(), c("species", "sites", "data_names", "flowers", "covers",
          "data_names_resolved", "traits_zirbel")
  ))


### c Summarize duplicates of traits matrix ------------------------------------

traits <- data_names %>%
  rename("name_submitted" = "name") %>%
  left_join(
    data_names_resolved %>% select(name_submitted, accepted_name),
    by = "name_submitted"
  ) %>%
  select(-abb, -name_submitted) %>%
  group_by(accepted_name) %>%
  summarize(seeded = sum(seeded, na.rm = TRUE)) %>%
  left_join(
    data_names_resolved %>%
      select(
        accepted_name, taxonomic_status, accepted_name_rank,
        accepted_family, accepted_name_url, source
      ),
    by = "accepted_name"
  ) %>%
  mutate(
    accepted_name = if_else(accepted_name == "", "unknown", accepted_name)
    ) %>%
  add_count(accepted_name) %>%
  filter(
    (taxonomic_status == "Accepted" & n == 2) | n == 1
      ) %>%
  distinct() %>%
  select(-n)


### d Summarize duplicates of species matrix -----------------------------------

data_species <- species %>%
  select(-seeded) %>%
  rename(abb = name) %>%
  left_join(data_names %>% select(abb, name), by = "abb") %>% 
  rename("name_submitted" = "name") %>%
  left_join(
    data_names_resolved %>% select(name_submitted, accepted_name),
    by = "name_submitted"
  ) %>%
  mutate(
    accepted_name = if_else(
      name_submitted == "unknown", "unknown", accepted_name
      )
    ) %>%
  select(
    id_plot_year, id_plot, plot_size, date_surveyed, accepted_name, abundance
    ) %>%
  ### Merge the following species to the genus level ###
  mutate(
    accepted_name = if_else(
      str_detect(accepted_name, "Digitaria"), "Digitaria", if_else(
        str_detect(accepted_name, "Melilotus"), "Melilotus", if_else(
          str_detect(accepted_name, "Rhus"), "Rhus", if_else(
            str_detect(accepted_name, "Setaria"), "Setaria", if_else(
              str_detect(accepted_name, "Ulmus"), "Ulmus", if_else(
                str_detect(accepted_name, "Vicia"), "Vicia", if_else(
                  str_detect(accepted_name, "Vitis"), "Vitis", accepted_name
                ))))))),
    accepted_name = if_else(accepted_name == "", "unknown", accepted_name)
    )

data_check_duplicated <- data_species %>%
  group_by(id_plot, plot_size, date_surveyed, accepted_name) %>%
  add_count(accepted_name) %>%
  arrange(desc(n), accepted_name, date_surveyed, id_plot, plot_size) %>%
  filter(n > 1)

species <- data_species %>%
  group_by(id_plot_year, id_plot, plot_size, date_surveyed, accepted_name) %>%
  summarize(abundance = sum(abundance)) %>%
  mutate(abundance = round(abundance, digits = 2)) %>%
  arrange(id_plot, date_surveyed, plot_size, accepted_name) %>%
  group_by(id_plot, date_surveyed, accepted_name) %>%
  slice(1) %>%
  arrange(id_plot, date_surveyed, plot_size, accepted_name) %>%
  ungroup()

data_check_duplicated <- species %>%
  group_by(id_plot, date_surveyed, accepted_name) %>%
  add_count(accepted_name) %>%
  arrange(desc(n), accepted_name, date_surveyed, id_plot)

data_check_occurences <- data_species %>%
  filter(
    plot_size != "seeded",
    #str_detect(id_plot_year, "_2025")
    ) %>%
  group_by(accepted_name) %>%
  count(accepted_name) %>%
  arrange(accepted_name, n)

# write_xlsx(
#   data_check_occurences,
#   here("data", "processed", "data_processed_occurences_20250818.xlsx")
#   )

rm(list = setdiff(
  ls(), c("species", "sites", "traits", "flowers", "covers", "traits_zirbel",
          "data_check_occurences")
  ))



## 2 Traits from databases ####################################################


### a Load traits from GIFT ---------------------------------------------------

trait_ids <- c("1.2.2", "1.6.3", "3.2.3", "4.1.3")

# GIFT::GIFT_traits_meta() %>%
#   filter(Lvl3 %in% trait_ids) %>%
#   tibble()

# Code takes a lot of time --> Load instead the data_gift file below:

# data_gift <- GIFT::GIFT_traits(
#   trait_IDs = trait_ids,
#   agreement = 0.66, bias_ref = FALSE, bias_deriv = FALSE
# )
# 
# write_csv(
#   data_gift,
#   here("data", "processed", "data_gift.csv")
# )

data_gift <- read_csv(
  here("data", "processed","data_gift.csv"),
  col_types = cols(
    .default = "?",
    cv_4.1.3 = "d",
    references_4.1.3 = "c"
    ),
  na = c("NA", "", "na")
  )


### b Combine GIFT and traits -------------------------------------------------

data_traits <- traits %>%
  left_join(
    data_gift %>%
      rename(accepted_name = work_species) %>%
      mutate(
        accepted_name = str_replace(
          accepted_name, "Andropogon gerardi", "Andropogon gerardii"
        ),
        accepted_name = str_replace(
          accepted_name, "Argentina anserina", "Potentilla anserina"
        ),
        accepted_name = str_replace(
          accepted_name, "Taraxacum ruderalia", "Taraxacum officinale"
        )
      ) %>% 
      select(
        accepted_name, trait_value_1.6.3, trait_value_3.2.3, trait_value_4.1.3,
        trait_value_1.2.2
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

data_traits %>% filter(duplicated(accepted_name))


### c Check completeness -------------------------------------------------------

data_missing <- data_traits %>%
  filter(
    (is.na(sla) | is.na(height) | is.na(seedmass)) &
      accepted_name_rank == "species"
    ) %>%
  select(accepted_name, seeded, family, sla, height, seedmass) %>%
  arrange(desc(seeded))


### d Combine Zirbel data and traits -------------------------------------------

data_traits2 <- data_traits %>%
  left_join(
    traits_zirbel %>% rename(accepted_name = name), by = "accepted_name"
  ) %>%
  mutate(
    sla = dplyr::coalesce(sla, sla_2017, sla_2020),
    height = dplyr::coalesce(height, height_2017),
    seedmass = dplyr::coalesce(seedmass, seedmass_2017, seedmass_2020)
  ) %>%
  select(-ends_with("_2017"), -ends_with("_2020"))


### e Check completeness -------------------------------------------------------

data_missing <- data_traits2 %>%
  filter(
    (is.na(sla) | is.na(height) | is.na(seedmass)) &
      accepted_name_rank == "species"
  ) %>%
  select(accepted_name, seeded, family, sla, height, seedmass) %>%
  arrange(desc(seeded))


### f TRY database -------------------------------------------------------------

data_try <- rtry_import(
  here("data", "raw", "database_try_43371", "43371.txt")
  ) %>%
  filter(!(is.na(TraitID))) %>%
  select(
    SpeciesName, AccSpeciesID, TraitID, TraitName, DataName, DataID, StdValue,
    UnitName, LastName, Reference
  ) %>%
  mutate(
    TraitID = str_replace(TraitID, "3117", "3115"),
    TraitID = str_replace(TraitID, "3116", "3115"),
    SpeciesName = str_replace(
      SpeciesName, "Lespedeza virginica Britton Orthodox", "Lespedeza virginica"
      ),
    SpeciesName = str_replace(
      SpeciesName, "Solidago namoralis", "Solidago nemoralis"
      ),
    SpeciesName = str_replace(
      SpeciesName, "Soldago nemoralis", "Solidago nemoralis"
      ),
    SpeciesName = str_replace(SpeciesName, "Zizia cordata", "Zizia aptera"),
    SpeciesName = str_replace(
      SpeciesName, "Desmodium illinoense A.Gray Orthodox p",
      "Desmodium illionense"
      ),
    SpeciesName = str_replace(
      SpeciesName, "Desmodium illinoensis", "Desmodium illionense"),
    SpeciesName = str_replace(
      SpeciesName, "Aster azureus", "Symphyotrichum oolentangiense"),
    SpeciesName = str_replace(
      SpeciesName, "Symphiotrichum oolentangiensis",
      "Symphyotrichum oolentangiense"
      ),
    SpeciesName = str_replace(
      SpeciesName, "Aster ericoides", "Symphyotrichum ericoides"),
    SpeciesName = str_replace(
      SpeciesName, "Symphyotrichum undulatum \\(L\\.\\) G\\.L\\.Nesom",
      "Symphyotrichum undulatum"
      ),
    SpeciesName = str_replace(
      SpeciesName, "Aster undulatum", "Symphyotrichum undulatum"
      ),
    SpeciesName = str_replace(
      SpeciesName, "Aster sagittifolius", "Symphyotrichum undulatum"
      )
    ) %>% 
  group_by(AccSpeciesID, TraitID, UnitName) %>%
  summarize(
    accepted_name = first(SpeciesName), mean = mean(StdValue, na.rm = TRUE)
    ) %>%
  mutate(
    TraitID = str_replace(TraitID, "26", "seedmass_new"),
    TraitID = str_replace(TraitID, "3106", "height_new"),
    TraitID = str_replace(TraitID, "3115", "sla_new")
  ) %>%
  ungroup() %>%
  select(-AccSpeciesID, -UnitName) %>%
  pivot_wider(names_from = "TraitID", values_from = "mean")


### g Combine TRY and traits --------------------------------------------------

data_traits3 <- data_traits2 %>%
  left_join(data_try, by = "accepted_name") %>%
  mutate(
    sla = dplyr::coalesce(sla, sla_new),
    height = dplyr::coalesce(height, height_new),
    seedmass = dplyr::coalesce(seedmass, seedmass_new)
  ) %>%
  select(-ends_with("_new"))


### h Check completeness -------------------------------------------------------

data_missing <- data_traits3 %>%
  filter(
    (is.na(sla) | is.na(height) | is.na(seedmass)) &
      accepted_name_rank == "species"
  ) %>%
  select(accepted_name, seeded, family, sla, height, seedmass) %>%
  left_join(data_check_occurences, by = "accepted_name") %>%
  relocate(n, .after = seeded) %>%
  filter((is.na(sla) | is.na(height) | is.na(seedmass))) %>%
  arrange(desc(seeded), desc(n))


# write_xlsx(
#   data_missing,
#   here("data", "processed", "data_processed_missing_20250818.xlsx")
# )

traits <- data_traits3

rm(list = setdiff(
  ls(), c("species", "sites", "traits", "flowers", "covers")
  ))



## 3 Alpha diversity ##########################################################


### a Species richness --------------------------------------------------------

richness <- species %>%
  left_join(
    traits %>% select(accepted_name, seeded, taxonomic_status, family),
    by = "accepted_name"
    ) %>%
  mutate(id_plot_year_size = str_c(id_plot_year, plot_size, sep = "_")) %>%
  select(
    accepted_name, seeded, taxonomic_status, family, id_plot_year_size,
    id_plot_year, id_plot, plot_size, abundance
  ) %>%
  mutate(presence = 1) %>%
  filter(plot_size != "seeded") %>%
  group_by(id_plot_year_size, id_plot_year, id_plot, plot_size)

#### Total species richness ###
richness_total <- richness %>%
  summarise(species_richness = sum(presence)) %>%
  ungroup()

#### Target species (species richness) ###
richness_seeded <- richness %>%
  filter(seeded == 1) %>%
  summarise(seeded_richness = sum(presence)) %>%
  ungroup()

data <- richness_total %>% 
  left_join(
    richness_seeded %>%  select(id_plot_year_size, seeded_richness),
    by = "id_plot_year_size"
    ) %>%
  pivot_longer(
    cols = ends_with("richness"),
    names_to = "richness_type",
    values_to = "richness"
    ) %>%
  pivot_wider(names_from = "plot_size", values_from = "richness") %>%
  rename(richness_1qm = "1", richness_25qm = "25") %>%
  group_by(id_plot_year, id_plot, richness_type) %>%
  summarize(
    richness_1qm = sum(richness_1qm, na.rm = TRUE),
    richness_25qm = sum(richness_25qm, na.rm = TRUE)
    ) %>%
  right_join(sites, by = "id_plot") %>%
  mutate(
    year = str_extract(id_plot_year, "[:digit:][:digit:][:digit:][:digit:]")
  ) %>%
  select(
    id_plot_year, id_plot, site, year, herbicide, seeding_time, seeded_pool,
    richness_1qm, richness_25qm, treatment_id, treatment_description,
    everything()
  )
sites <- data

rm(list = setdiff(
  ls(), c("species", "sites", "traits", "coordinates","traits_zirbel")
  ))



## 4 Covers ###################################################################


### a Total cover -------------------------------------------------------------

data_cover_total <- species %>%
  group_by(id_plot_year) %>%
  summarise(cover_total = sum(abundance, na.rm = TRUE))

data_cover_seeded_grass <- species %>%
  left_join(
    traits %>% select(accepted_name, family, seeded),
    by = "accepted_name"
    ) %>%
  mutate(
    abundance = if_else(
      family == "Poaceae" & seeded == 1, abundance, 0
      )
    ) %>%
  group_by(id_plot_year) %>%
  summarise(cover_seeded_grass = sum(abundance, na.rm = TRUE))
  
data_cover_seeded_forbs<- species %>%
  left_join(
    traits %>% select(accepted_name, family, seeded),
    by = "accepted_name"
  ) %>%
  mutate(
    abundance = if_else(
      family != "Poaceae" & seeded == 1, abundance, 0
      )
    ) %>%
  group_by(id_plot_year) %>%
  summarise(cover_seeded_forbs = sum(abundance, na.rm = TRUE))

data_cover_non_seeded <- species %>%
  left_join(
    traits %>% select(accepted_name, seeded),
    by = "accepted_name"
  ) %>%
  mutate(abundance = if_else(seeded == 0, abundance, 0)) %>%
  group_by(id_plot_year) %>%
  summarise(cover_non_seeded = sum(abundance, na.rm = TRUE))

sites <- sites %>%
  full_join(data_cover_total, by = "id_plot_year") %>%
  full_join(data_cover_seeded_grass, by = "id_plot_year") %>%
  full_join(data_cover_seeded_forbs, by = "id_plot_year") %>%
  full_join(data_cover_non_seeded, by = "id_plot_year")

rm(list = setdiff(
  ls(), c("species", "sites", "traits", "coordinates","traits_zirbel")
  ))



## 5 CWM of SLA ----------------------------------------------------------------


### a preparation --------------------------------------------------------------

# data <- traits %>%
#   filter(seeded == 1) %>%
#   select(accepted_name, sla)
# 
# data %>%
#   naniar::miss_var_summary(order = TRUE)
# 
# data_traits <- data %>%
#   filter(!is.na(sla)) %>%
#   mutate(sla = log(sla))
# 
# data_species <- species %>%
#   ungroup() %>%
#   select(id_plot_year, plot_size, accepted_name, abundance) %>%
#   semi_join(data_traits, by = "accepted_name") %>% 
#   filter(plot_size != "seeded") %>%
#   select(-plot_size)
# 
# # Check duplicates
# data_species %>%
#   group_by(id_plot_year, accepted_name) %>% 
#   count() %>%
#   filter(n > 1)
# 
# data_species <- data_species %>%
#   arrange(accepted_name) %>%
#   pivot_wider(
#     names_from = "accepted_name", values_from = "abundance", values_fill = 0
#   ) %>%
#   filter(!is.na(id_plot_year)) %>% # no NA in dataset
#   column_to_rownames("id_plot_year")
# 
# data_traits <- data_traits %>%
#   arrange(accepted_name) %>%
#   filter(!is.na(accepted_name)) %>% # no NA
#   column_to_rownames("accepted_name")
# 
# 
# ### b calculation --------------------------------------------------------------
# 
# # data_abundance <- FD::dbFD(
# #   data_traits, data_species,
# #   w.abun = TRUE, calc.FRic = TRUE, calc.FDiv = FALSE, corr = "sqrt"
# # )
# data_presence <- FD::dbFD(
#   data_traits, data_species,
#   w.abun = FALSE, calc.FRic = TRUE, calc.FDiv = FALSE,
# )
# 
# 
# ### c saving -------------------------------------------------------------------
# 
# data_abu <- data_abundance$FDis %>%
#   as.data.frame() %>%
#   add_column(data_abundance$CWM$sla) %>%
#   add_column(data_abundance$FRic) %>%
#   rownames_to_column("id.plot") %>%
#   rename(
#     "fdis.abu.sla" = ".",
#     "fric.abu.sla" = "data_abundance$FRic",
#     "cwm.abu.sla" = "data_abundance$CWM$sla"
#   ) %>%
#   mutate(
#     across(where(is.numeric), ~ exp(.x)),
#     across(where(is.numeric), ~ round(.x, digits = 2))
#   ) %>%
#   select(id.plot, fdis.abu.sla, fric.abu.sla, cwm.abu.sla)
# 
# data_pres <- data_presence$FDis %>%
#   as.data.frame() %>%
#   add_column(data_presence$CWM$sla) %>%
#   add_column(data_presence$FRic) %>%
#   rownames_to_column("id.plot") %>%
#   rename(
#     "fdis.pres.sla" = ".",
#     "fric.pres.sla" = "data_presence$FRic",
#     "cwm.pres.sla" = "data_presence$CWM$sla"
#   ) %>%
#   mutate(
#     across(where(is.numeric), ~ exp(.x)),
#     across(where(is.numeric), ~ round(.x, digits = 2))
#   ) %>%
#   select(id.plot, fdis.pres.sla, fric.pres.sla, cwm.pres.sla)
# 
# sla <- data_abu %>%
#   full_join(data_pres, by = "id.plot")
# 
# rm(list = setdiff(ls(), c(
#   "species", "traits", "environment", "veg_head", "forb_grass_ratio", "biomass",
#   "height_max", "species_list", "oek_f", "oek_l", "sla"
# )))



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
