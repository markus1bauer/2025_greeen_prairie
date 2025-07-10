#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GREEEN prairie experiment
# Prepare data ####
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2025-07-07



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
  select(-year)

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
  select(-species_pool) %>%
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
  left_join(sites %>% select(id_plot, species_pool), by = "id_plot") %>%
  select(
    id_plot_year, id_plot, plot_size, species_pool, date_surveyed, date_entered,
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
    species_pool
    ) %>%
  select(-date_entered, -species_pool)

rm(list = setdiff(ls(), c("species", "sites", "flowers", "covers")))



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Create variables ###########################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## 1 Names from TNRS database #################################################


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
    name = str_replace(name, "ARAGLA", "Arabis glabra"),
    name = str_replace(name, "ARATHA", "Arabidopsis thaliana"),
    name = str_replace(name, "ASCAMP", "Asclepias amplexicaulis"),
    name = str_replace(name, "ASCSYR", "Asclepias syriaca"),
    name = str_replace(name, "ASCTUB", "Asclepias tuberosa"),
    name = str_replace(name, "ASPOFF", "Asparagus officinalis"),
    name = str_replace(name, "ASTCAN", "Astragalus canadensis"),
    name = str_replace(name, "ASTERI", "Aster ericoides"),
    name = str_replace(name, "ASTLAE", "Aster laevis"),
    name = str_replace(name, "ASTOOL", "Aster oolentangiensis"),
    name = str_replace(name, "ASTPIL", "Aster pilosus"),
    name = str_replace(name, "ASTSAG", "Aster sagittifolius"),
    name = str_replace(name, "AVESAT", "Avena sativa"),
    name = str_replace(name, "BAPLAC", "Baptisia lactea"),
    name = str_replace(name, "BARVUL", "Barbarea vulgaris"),
    name = str_replace(name, "BERINC", "Berteroa incana"),
    name = str_replace(name, "BERVUL", "Berberis vulgaris"),
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
    name = str_replace(name, "DESSPP", "Desmodium sp."),
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
    name = str_replace(name, "FESTRA", "Festuca trachyphylla"),
    name = str_replace(name, "GERSPP", "Geranium sp."),
    name = str_replace(name, "GEUCAN", "Geum canadense"),
    name = str_replace(name, "GLETRI", "Gleditsia triacanthos"),
    name = str_replace(name, "GNAOBT", "Gnaphalium obtusifolium"),
    name = str_replace(name, "GNAPUR", "Gnaphalium purpureum"),
    name = str_replace(name, "GNASPP", "Gnaphalium sp."),
    name = str_replace(name, "GNAULI", "Gnaphalium uliginosum"),
    name = str_replace(name, "HACVIR", "Hackelia virginiana"),
    name = str_replace(name, "HELOCC", "Helianthemum occidentale"),
    name = str_replace(name, "HIESPP", "Hieracium sp."),
    name = str_replace(name, "HYPPER", "Hypericum perforatum"),
    name = str_replace(name, "HYPRAD", "Hypochaeris radicata"),
    name = str_replace(name, "JUNEFF", "Juncus effusus"),
    name = str_replace(name, "JUNTEN", "Juncus tenuis"),
    name = str_replace(name, "KOEMAC", "Koeleria macrantha"),
    name = str_replace(name, "KRISPP", "Krigia sp."),
    name = str_replace(name, "LACSPP", "Lactuca sp."),
    name = str_replace(name, "LEPCAM", "Lepidium campestre"),
    name = str_replace(name, "LESCAP", "Lespedeza capitata"),
    name = str_replace(name, "LESHIR", "Lespedeza hirta"),
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
    name = str_replace(name, "PHYSPP", "Phytolacca"),
    name = str_replace(name, "PLALAN", "Plantago lanceolata"),
    name = str_replace(name, "PLAMAJ", "Plantago major"),
    name = str_replace(name, "POAANN", "Poa annua"),
    name = str_replace(name, "POACOM", "Poa compressa"),
    name = str_replace(name, "POAPRA", "Poa pratensis"),
    name = str_replace(name, "POASPP", "Poa sp."),
    name = str_replace(name, "POTARGE", "Potentilla argentilla"),
    name = str_replace(name, "POTNOR", "Potentilla norvegica"),
    name = str_replace(name, "POTREC", "Potentilla recta"),
    name = str_replace(name, "POTSIM", "Potentilla simplex"),
    name = str_replace(name, "PRUCUL", "Prunus cultivated"),
    name = str_replace(name, "PRUSER", "Prunus serotina"),
    name = str_replace(name, "PRUSPP", "Prunus sp."),
    name = str_replace(name, "PSEOBT", "Pseudognaphalium obtusifolium"),
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
    name = str_replace(name, "SOLSPE", "Solidago speciosa"),
    name = str_replace(name, "SOLSPP", "Solidago sp."),
    name = str_replace(name, "SORNUT", "Sorghastrum nutans"),
    name = str_replace(name, "SPOCRY", "Sporobolus cryptandrus"),
    name = str_replace(name, "STEMED", "Stellaria media"),
    name = str_replace(name, "SYMERI", "Symphyotrichum ericoides"),
    name = str_replace(name, "SYMLAE", "Symphyotrichum laeve"),
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
  rename_with(tolower)

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
  arrange(desc(n), accepted_name)

rm(list = setdiff(ls(), c("species", "sites", "data_names", "flowers", "covers",
                          "data_names_resolved")))


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
  arrange(desc(n), accepted_name, date_surveyed, id_plot, plot_size)

species <- data_species %>%
  group_by(id_plot_year, id_plot, plot_size, date_surveyed, accepted_name) %>%
  summarize(abundance = sum(abundance)) %>%
  mutate(abundance = round(abundance, digits = 2)) %>%
  arrange(id_plot, date_surveyed, plot_size, accepted_name) %>%
  group_by(id_plot, date_surveyed, accepted_name) %>%
  slice(1) %>%
  arrange(id_plot, date_surveyed, plot_size, accepted_name)

data_check_duplicated <- species %>%
  group_by(id_plot, date_surveyed, accepted_name) %>%
  add_count(accepted_name) %>%
  arrange(desc(n), accepted_name, date_surveyed, id_plot)

data_check_occurences <- data_species %>%
  group_by(accepted_name) %>%
  count(accepted_name) %>%
  arrange(accepted_name, n)

# write_xlsx(
#   data_check_occurences,
#   here("data", "processed", "data_processed_occurences_20250704.xlsx")
#   )

rm(list = setdiff(ls(), c("species", "sites", "traits", "flowers", "covers")))



## 2 Traits from GIFT database ################################################


### a Load traits from GIFT ---------------------------------------------------

trait_ids <- c("1.2.2", "1.6.3", "3.2.3", "4.1.3")

GIFT::GIFT_traits_meta() %>%
  filter(Lvl3 %in% trait_ids) %>%
  tibble()

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

data_gift <- read_csv(here("data", "processed","data_gift.csv"))


### b Combine gift and traits -------------------------------------------------

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

data_missing <- data_traits %>%
  filter(
    (is.na(sla) | is.na(height) | is.na(seedmass)) &
      accepted_name_rank == "species"
    ) %>%
  select(accepted_name, seeded, family, sla, height, seedmass)

# write_xlsx(
#   data_missing,
#   here("data", "processed", "data_processed_missing_20250709.xlsx")
# )

# gift %>%
#   filter(str_detect(accepted_name, "Lolium pratense")) %>%
#   select(1:2, starts_with("trait"))
# 
# traits <- data_summarized %>%
#   mutate(
#     across(c("R1A", "R22", "both"), replace_na, 0),
#     sla = if_else(accepted_name == "Festuca nigrescens", 193.57, sla), # Value of Festuca rubra (GIFT traitbase)
#     sla = if_else(accepted_name == "Lolium pratense", 214.00, sla), # Value of LEDA traitbase
#     sla = if_else(accepted_name == "Festuca rupicola", 193.57, sla), # Value of Festuca rubra (GIFT traitbase)
#     sla = if_else(accepted_name == "Medicago falcata", 219.5, sla), # Value of Medicago sativa (GIFT traitbase)
#     height = if_else(accepted_name == "Lolium pratense", 0.55, height), # Value of LEDA traitbase
#     height = if_else(accepted_name == "Medicago falcata", 0.52, height), # Value of Medicago sativa (GIFT traitbase)
#     seedmass = if_else(
#       accepted_name == "Leucanthemum ircutianum", 0.0008119666, seedmass
#       ) # Value of Leucanthemum vulgare
#     )


rm(list = setdiff(ls(), c("species", "sites", "traits", "flowers", "covers")))



## 3 Alpha diversity ##########################################################


### a Species richness -------------------------------------------------------

richness <- species %>%
  left_join(
    traits %>% select(accepted_name, seeded, taxonomic_status, accepted_family),
    by = "accepted_name"
    ) %>%
  mutate(id_plot_year_size = str_c(id_plot_year, plot_size, sep = "_")) %>%
  select(
    accepted_name, seeded, taxonomic_status, accepted_family, id_plot_year_size,
    id_plot_year, id_plot, plot_size, abundance
  ) %>%
  mutate(presence = 1) %>%
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

sites <- richness_total %>% 
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
  left_join(sites, by = "id_plot") %>%
  mutate(
    year = str_extract(id_plot_year, "[:digit:][:digit:][:digit:][:digit:]")
  ) %>%
  rename(pool_seeded = "seeded", pool_1 = "1", pool_25 = "25") %>%
  select(
    id_plot_year, id_plot, site, year, herbicide, seeding_time, species_pool,
    treatment_id, treatment_description, pool_seeded, pool_1, pool_25,
    everything()
  )


### b Species eveness ---------------------------------------------

# data <- species_dikes %>%
#   mutate(across(where(is.numeric), ~ replace(., is.na(.), 0))) %>%
#   pivot_longer(-name, names_to = "id", values_to = "value") %>%
#   pivot_wider(names_from = "name", values_from = "value") %>%
#   column_to_rownames("id") %>%
#   diversity(index = "shannon") %>%
#   as_tibble(rownames = NA) %>%
#   rownames_to_column(var = "id") %>%
#   mutate(id = factor(id)) %>%
#   rename(shannon = value)
# sites_dikes <- sites_dikes %>%
#   left_join(data, by = "id") %>%
#   mutate(eveness = shannon / log(species_richness))

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
