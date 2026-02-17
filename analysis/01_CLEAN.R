### setup ####

library(tidyverse)
library(janitor)
library(stringr)

### load data ####

sightings <- read_csv("data_raw/validated_sightings.csv") %>%
  clean_names()

glimpse(sightings)

### filter valid sightings ####

sightings_valid <- sightings %>%
  filter(validation == "valid")

nrow(sightings_valid)
count(sightings, validation)

### basic data exploration ####

sightings_valid %>%
  count(species, sort = TRUE)

sightings_valid %>%
  count(dive_site, sort = TRUE)

### fix missing dive sites (URL-level) ####

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
  select(-dive_site_fix) %>%
  filter(!is.na(dive_site))

### site lookup table ####

sites_raw <- sightings_valid %>%
  count(dive_site, name = "n_sightings") %>%
  arrange(desc(n_sightings))

print(sites_raw, n = Inf)

site_lookup <- sites_raw %>%
  mutate(
    dive_site_std = NA_character_,
    site_type     = NA_character_,   # site | region_label
    country       = NA_character_,   # Thailand | Myanmar
    region        = NA_character_
  )

write_csv(site_lookup, "data_clean/site_lookup_draft.csv")

### join edited site lookup ####

site_lookup <- read_csv("data_clean/site_lookup_edited.csv")

site_lookup <- site_lookup %>%
  distinct(dive_site, .keep_all = TRUE)

sightings_valid <- sightings_valid %>%
  left_join(
    site_lookup %>%
      select(dive_site, dive_site_std, site_type, country, region),
    by = "dive_site"
  )

### species lookup table ####

species_raw <- sightings_valid %>%
  distinct(species) %>%
  arrange(species)

species_lookup <- species_raw %>%
  mutate(
    species_std       = NA_character_,
    genus             = NA_character_,
    species_epithet   = NA_character_,
    scientific_name   = NA_character_
  )

write_csv(species_lookup, "data_clean/species_lookup_draft.csv")

### load and clean edited species lookup ####

species_lookup <- read_csv(
  "data_clean/species_lookup_edited.csv",
  locale = locale(encoding = "UTF-8"),
  col_types = cols(
    species = col_character(),
    group = col_character(),
    species_std = col_character(),
    genus = col_character(),
    species_epithet = col_character(),
    scientific_name = col_character()
  )
) %>%
  mutate(
    genus = iconv(genus, from = "", to = "UTF-8", sub = ""),
    species_epithet = iconv(species_epithet, from = "", to = "UTF-8", sub = ""),
    genus = str_trim(genus),
    species_epithet = str_trim(species_epithet),
    species_epithet = str_replace_all(species_epithet, "[^A-Za-z]", ""),
    scientific_name = if_else(
      genus != "" & species_epithet != "",
      paste(genus, tolower(species_epithet)),
      NA_character_
    )
  ) %>%
  distinct(species, .keep_all = TRUE)

### join species lookup ####

sightings_valid <- sightings_valid %>%
  left_join(
    species_lookup %>%
      select(species, species_std, genus, species_epithet, scientific_name, group),
    by = "species"
  )

### post-clean checks ####

sightings_valid %>%
  count(species_std, sort = TRUE)

sightings_valid %>%
  count(dive_site_std, sort = TRUE)

sightings_valid %>%
  count(region, sort = TRUE)

### save cleaned data ####

write_csv(
  sightings_valid,
  "data_clean/validated_sightings_clean.csv"
)





### build final elasmos dataset ####

elasmos <- sightings_valid %>%
  mutate(
    species_std     = iconv(species_std, from = "", to = "UTF-8", sub = ""),
    genus           = iconv(genus, from = "", to = "UTF-8", sub = ""),
    species_epithet = iconv(species_epithet, from = "", to = "UTF-8", sub = ""),
    scientific_name = iconv(scientific_name, from = "", to = "UTF-8", sub = ""),
    dive_site_std   = iconv(dive_site_std, from = "", to = "UTF-8", sub = ""),
    region          = iconv(region, from = "", to = "UTF-8", sub = ""),
    country         = iconv(country, from = "", to = "UTF-8", sub = ""),
    
    group   = str_trim(group),
    species = str_trim(species_std),
    genus   = str_trim(genus),
    species_epithet = str_trim(species_epithet),
    scientific_name = str_trim(scientific_name),
    dive_site = str_trim(dive_site_std),
    region = str_trim(region),
    country = str_to_title(str_trim(country)),
    
    sighting_date_raw = str_trim(as.character(sighting_date))
  ) %>%
  transmute(
    trip_id = url,   
    species, genus, species_epithet, scientific_name, group,
    country, region, dive_site, site_type,
    sighting_date_raw,
    n_indiv = n_observed
  ) %>%
  filter(country != "India", species != "minke whale") %>%
  mutate(
    ### patch partial/invalid dates (your rules) ####
    sighting_date_raw = case_when(
      # NEW: trip-level overrides
      trip_id == "https://www.thesmilingseahorse.com/blog/trip-report-north-and-south-andaman-christmas-cruise" ~ "2023-12-19",
      trip_id == "https://www.thesmilingseahorse.com/blog/why-we-love-burma-have-a-look-below" ~ "2019-03-03",
      is.na(sighting_date_raw) ~ NA_character_,
      sighting_date_raw == "2024-03-010" ~ "2024-03-10",
      str_detect(sighting_date_raw, "^\\d{4}$") ~ paste0(sighting_date_raw, "-01-01"),
      str_detect(sighting_date_raw, "^\\d{4}-\\d{2}$") ~ paste0(sighting_date_raw, "-01"),
      sighting_date_raw == "04/31/2024" ~ "04/30/2024",
      sighting_date_raw == "2025-02-29" ~ "2025-02-28",
      sighting_date_raw == "2023-02-29" ~ "2023-02-28",
      TRUE ~ sighting_date_raw
    ),
    
    ### parse once ####
    sighting_date = lubridate::parse_date_time(
      sighting_date_raw,
      orders = c("ymd", "mdy", "dmy"),
      exact = FALSE
    ) %>% as.Date(),
    
    year  = lubridate::year(sighting_date),
    month = lubridate::month(sighting_date),
    ym    = as.Date(sprintf("%d-%02d-01", year, month))
  )


### fix formats ####

elasmos <- elasmos %>%
  mutate(
    trip_id = factor(trip_id),
    group = factor(group),
    species = factor(species),
    scientific_name = factor(scientific_name),
    country = factor(country),
    region = factor(region),
    dive_site = factor(dive_site),
    site_type = factor(site_type, levels = c("site", "region")),
    year = as.integer(year),
    month = as.integer(month),
    n_indiv = as.numeric(n_indiv)
  )



# fix a couple bad dates 
elasmos %>%
  group_by(trip_id) %>%
  summarise(min_date = min(sighting_date, na.rm = TRUE),
            max_date = max(sighting_date, na.rm = TRUE),
            span_days = as.numeric(max_date - min_date)) %>%
  arrange(desc(span_days)) %>%
  slice(1:20)

trip_2023 <- "https://www.thesmilingseahorse.com/blog/9th-to-17th-of-december-2023-mergui-archipelago-and-burma-banks-a-dive-adventure"


elasmos <- elasmos %>%
  mutate(
    sighting_date = if_else(
      trip_id == trip_2023,
      make_date(2023, month(sighting_date), day(sighting_date)),
      sighting_date
    )
  ) %>%
  mutate(
    year  = year(sighting_date),
    month = month(sighting_date),
    ym    = as.Date(sprintf("%d-%02d-01", year, month))
  )

bad_trip_2 <- "https://www.thesmilingseahorse.com/blog/-blotched-stingray-vs-nurse-shark"

elasmos <- elasmos %>%
  mutate(
    sighting_date = if_else(
      trip_id == bad_trip_2,
      make_date(2012, month(sighting_date), day(sighting_date)),
      sighting_date
    ),
    year  = year(sighting_date),
    month = month(sighting_date),
    ym    = as.Date(sprintf("%d-%02d-01", year, month))
  )



write_csv(elasmos, "data_clean/elasmos_sightings.csv")

str(elasmos)
count(elasmos, country)

