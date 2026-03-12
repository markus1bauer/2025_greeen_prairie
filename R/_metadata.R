#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GREEEN prairie experiment
# Metadata ####
# 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2026-02-25


### Packages ###
library(here)
library(tidyverse)
library(EML)

### Start ###
rm(list = ls())



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Collect metadata ##########################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## 1 Methods and units #######################################################


#methods_file <- here("data", "text", "methods.odt")
#methods <- set_methods(methods_file)

# remotes::install_github("EDIorg/EMLassemblyline")
# EMLassemblyline::view_unit_dictionary()
# List of standard units, which should be used in metadata file



## 2 Raw data ################################################################

## 3 Processed data ##########################################################

## 4 Put data table together #################################################



## 5 Contact #################################################################


address <- list(
  deliveryPoint = "Emil-Ramann-Strasse 6",
  city = "Freising",
  administrativeArea = "Bavaria",
  postalCode = "85354",
  country = "Germany"
  )

creator <- EML::eml$creator(
  individualName = EML::eml$individualName(
    givenName = "Markus",
    surName = "Bauer"
  ),
  positionName = "Researcher",
  organizationName = "Technical University of Munich",
  address = address,
  electronicMailAddress = "markus1.bauer@tum.de",
  phone = "0049-152-56391781",
  id = "https://orcid.org/0000-0001-5372-4174"
)

associatedParty <- list(
  EML::eml$associatedParty(
    individualName = EML::eml$individualName(
      givenName = "Anna M.",
      surName = "Groves"
    ),
    role = "Researcher",
    organizationName = "Ampliflora",
    electronicMailAddress = "itsdrfunk@gmail.com",
    id = "https://orcid.org/0000-0002-4063-197X"
  ),
  EML::eml$associatedParty(
    individualName = EML::eml$individualName(
      givenName = "Rufus",
      surName = "Isaacs"
    ),
    role = "Researcher",
    organizationName = "Michigan State University",
    electronicMailAddress = "isaacsr@msu.edu",
    id = "https://orcid.org/0000-0001-7523-4643"
  ),
  EML::eml$associatedParty(
    individualName = EML::eml$individualName(
      givenName = "Douglas A.",
      surName = "Landis"
    ),
    role = "Researcher",
    organizationName = "Michigan State University",
    electronicMailAddress = "landisd@msu.edu",
    id = "https://orcid.org/0000-0003-4943-6000"
  ),
  EML::eml$associatedParty(
    individualName = EML::eml$individualName(
      givenName = "Lars A.",
      surName = "Brudvig"
    ),
    role = "Researcher",
    organizationName = "Michigan State University",
    electronicMailAddress = "brudvig@msu.edu",
    id = "https://orcid.org/0000-0002-3857-2165"
  )
)

contact <- list(
  individualName = creator$individualName,
  electronicMailAddress = creator$electronicMailAddress,
  address = address,
  organizationName = "Technical University of Munich",
  onlineUrl = "https://www.lss.ls.tum.de/en/roek/home/"
)



## 6 Temporal and spatial coverage ###########################################


geographic_description <- "Three experiments in Michigan/USA"

coverage <- set_coverage(
  begin = "2015-03-01", end = "2025-07-31",
  sci_names = list(list(
    Subdivision = "Spermatophytina"
  )),
  geographicDescription = geographic_description,
  west = -86.3590, east = -85.44857,
  north = 44.82026, south = 42.0840
)



## 7 Description #############################################################


title <- "Tallgrass restoration experiment with different species pool sizes and different seeding seasons"

pubDate <- "2026"

alternate_identifier <- ""

abstract <- "There are three experimental sites in Michigan with 60 plots each. We conducted per plot vegetation surveys. The treatments were a herbicide treatment, planting season (autumn or spring) and the seeded pool size (6, 12, 18, 33). The monitoring lasted from 2015 to 2025 at one site and from 2015 and 2018 at two sites. We measured some own leaves to get specific leaf area."


# LTER controlled vocabulary
# https://vocab.lternet.edu/vocab/vocab/index.php?_search_expresion=vegetation
keyword_set <- list(
  list(
    keywordThesaurus = "LTER controlled vocabulary",
    keyword = list(
      "grasslands",
      "human disturbance",
      "plant communities",
      "plant height",
      "plant species composition",
      "restoration",
      "seeds",
      "species richness",
      "specific leaf area",
      "vegetation"
      )
  ),
  list(
    keywordThesaurus = "own vocabulary",
    keyword = list(
      "RLQ",
      "community assembly",
      "community ecology",
      "plant functional traits",
      "species pool size",
      "tallgrass prairie",
      "target species richness",
      "temperate grassland"
      )
  )
)

license <- list(
  licenseName = "CC-BY-4.0",
  url = "https://creativecommons.org/licenses/by/4.0/deed.en"
)

short_name <- "GREEEN: prairie restoration with different pool sizes and planting season"

language <- "English"



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B finalize EML ##############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



dataset <- list(
  title = title,
  shortName = short_name,
  pubDate = pubDate,
  creator = creator,
  associatedParty = associatedParty,
  licensed = license,
  alternateIdentifier = alternate_identifier,
  abstract = abstract,
  keywordSet = keyword_set,
  coverage = coverage,
  language = language,
  contact = contact#,
  #methods = methods,
  #dataTable = dataTable,
  #additonalMetadata = list(metadata = list(unitList = unitList))
  )

eml <- list(
  packageId = uuid::UUIDgenerate(),
  system = "uuid", # type of identifier
  dataset = dataset
  )

EML::write_eml(eml, here("METADATA.xml"))
EML::eml_validate(here("METADATA.xml"))
