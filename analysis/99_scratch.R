### basic data exploration ####

library(tidyverse)
library(janitor)

sightings <- read_csv("data_raw/validated_sightings.csv") %>%
  clean_names()

glimpse(sightings)

names(sightings)

sightings_valid <- sightings %>%
  filter(validation == "valid")

nrow(sightings_valid)


count(sightings, validation)

sightings_valid %>%
  count(species, sort = TRUE)

sightings_valid %>%
  count(dive_site, sort = TRUE)

# clean dive sites names 

sites_raw <- sightings_valid %>%
  count(dive_site, name = "n_sightings") %>%
  arrange(dive_site)
print(sites_raw, n = Inf)

# check NAs amd fill 


na_site_fixes <- tibble(
  url = c(
    "https://www.thesmilingseahorse.com/blog/trip-report-north-and-south-andaman-christmas-cruise",
    "https://www.thesmilingseahorse.com/blog/welcome-to-our-lucky-belgium-charter",
    "https://www.thesmilingseahorse.com/blog/welcome-to-the-mantas-parade"
  ),
  dive_site_fix = c(
    "Andaman Sea",
    "Mergui Archipelago",
    "Mergui Archipelago"
  )
)

sightings_valid <- sightings_valid %>%
  left_join(na_site_fixes, by = "url") %>%
  mutate(dive_site = coalesce(dive_site, dive_site_fix)) %>%
  select(-dive_site_fix)

sightings_valid <- sightings_valid %>%
  filter(!is.na(dive_site))


### fix site and region names 

# create site lookup table 
site_lookup <- sites_raw %>%
  mutate(
    dive_site_std = NA_character_,
    country       = NA_character_,
    region        = NA_character_
  )

site_lookup <- sightings_valid %>%
  filter(!is.na(dive_site)) %>%
  distinct(dive_site) %>%
  arrange(dive_site) %>%
  mutate(
    dive_site_std = NA_character_,     # canonical name
    site_type     = NA_character_,     # "site" or "region_label"
    country       = NA_character_,     # "Thailand" / "Myanmar"
    region        = NA_character_      # e.g., "Similan", "Surin", "Koh Lipe", "Mergui", "Phuket/Racha", etc.
  )

write_csv(site_lookup, "data_raw/site_lookup.csv")

