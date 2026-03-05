### 01_CLEAN.R ####
### Purpose: Clean validated sightings, apply lookup tables, build final elasmos dataset, write clean outputs

### 00. SETUP ####

library(tidyverse)
library(janitor)
library(stringr)
library(lubridate)

# expects: analysis_dir from 00_RUN.R
stopifnot(exists("analysis_dir"))
stopifnot(dir.exists(analysis_dir))


# expects these from 00_RUN.R:
# - output_dir, eda_dir, summ_dir, tables_dir (etc.)
# always write inside analysis/
data_raw_dir   <- file.path(analysis_dir, "data_raw")
data_clean_dir <- file.path(analysis_dir, "data_clean")

dir.create(data_raw_dir,   showWarnings = FALSE, recursive = TRUE)
dir.create(data_clean_dir, showWarnings = FALSE, recursive = TRUE)


### 01. LOAD DATA ####

sightings <- read_csv(file.path(data_raw_dir, "validated_sightings.csv")) %>%
  clean_names()


### 02. FILTER VALID SIGHTINGS ####

sightings_valid <- sightings %>%
  filter(validation == "valid")

### 03. FIX MISSING DIVE SITES (URL-LEVEL) ####

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

### 04. SITE LOOKUP TABLES ####

sites_raw <- sightings_valid %>%
  count(dive_site, name = "n_sightings") %>%
  arrange(desc(n_sightings))

site_lookup <- sites_raw %>%
  mutate(
    dive_site_std = NA_character_,
    site_type     = NA_character_,  # site | region_label
    country       = NA_character_,  # Thailand | Myanmar
    region        = NA_character_
  )

# draft lookup for manual edit
write_csv(site_lookup, file.path(data_clean_dir, "site_lookup_draft.csv"))


# join edited lookup
site_lookup <- read_csv(file.path(data_clean_dir, "site_lookup_edited.csv")) %>%
  distinct(dive_site, .keep_all = TRUE)

sightings_valid <- sightings_valid %>%
  left_join(
    site_lookup %>% select(dive_site, dive_site_std, site_type, country, region),
    by = "dive_site"
  )

### 05. SPECIES LOOKUP TABLES ####

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

# draft lookup for manual edit
write_csv(species_lookup, file.path(data_clean_dir, "species_lookup_draft.csv"))

# load + clean edited lookup
species_lookup <- read_csv(
  file.path(data_clean_dir, "species_lookup_edited.csv"),
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

# join lookup
sightings_valid <- sightings_valid %>%
  left_join(
    species_lookup %>% select(species, species_std, genus, species_epithet, scientific_name, group),
    by = "species"
  )

### 06. SAVE CLEANED VALIDATED SIGHTINGS ####

write_csv(sightings_valid, file.path(data_clean_dir, "validated_sightings_clean.csv"))

### 07. BUILD FINAL ELASMOS DATASET ####

elasmos <- sightings_valid %>%
  mutate(
    species_std     = iconv(species_std, from = "", to = "UTF-8", sub = ""),
    genus           = iconv(genus, from = "", to = "UTF-8", sub = ""),
    species_epithet = iconv(species_epithet, from = "", to = "UTF-8", sub = ""),
    scientific_name = iconv(scientific_name, from = "", to = "UTF-8", sub = ""),
    scientific_name = case_when(
      scientific_name == "acroteriovatus spp" ~ "Acroteriobatus spp", # sp error 
      scientific_name == "Rhina anclyostoma" ~ "Rhina ancylostomus", # sp error 
      scientific_name == "Pastinachus sephen" ~ "Pastinachus ater", # updated to reflect current taxonomy
      TRUE ~ scientific_name
    ),
    dive_site_std   = iconv(dive_site_std, from = "", to = "UTF-8", sub = ""),
    region          = iconv(region, from = "", to = "UTF-8", sub = ""),
    country         = iconv(country, from = "", to = "UTF-8", sub = ""),
    
    group           = str_trim(group),
    species         = str_trim(species_std),
    genus           = str_trim(genus),
    species_epithet = str_trim(species_epithet),
    scientific_name = str_trim(scientific_name),
    dive_site       = str_trim(dive_site_std),
    region          = str_trim(region),
    country         = str_to_title(str_trim(country)),
    
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
    sighting_date_raw = case_when(
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
    sighting_date = lubridate::parse_date_time(
      sighting_date_raw,
      orders = c("ymd", "mdy", "dmy"),
      exact = FALSE
    ) %>% as.Date(),
    year  = lubridate::year(sighting_date),
    month = lubridate::month(sighting_date),
    ym    = as.Date(sprintf("%d-%02d-01", year, month))
  ) %>%
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

### 08. FIX TWO KNOWN BAD DATE CASES ####

trip_2023 <- "https://www.thesmilingseahorse.com/blog/9th-to-17th-of-december-2023-mergui-archipelago-and-burma-banks-a-dive-adventure"
bad_trip_2 <- "https://www.thesmilingseahorse.com/blog/-blotched-stingray-vs-nurse-shark"

elasmos <- elasmos %>%
  mutate(
    sighting_date = if_else(
      trip_id == trip_2023,
      make_date(2023, month(sighting_date), day(sighting_date)),
      sighting_date
    ),
    sighting_date = if_else(
      trip_id == bad_trip_2,
      make_date(2012, month(sighting_date), day(sighting_date)),
      sighting_date
    ),
    year  = year(sighting_date),
    month = month(sighting_date),
    ym    = as.Date(sprintf("%d-%02d-01", year, month))
  )

### 09. SAVE FINAL ELASMOS ####

write_csv(elasmos, file.path(data_clean_dir, "elasmos_sightings.csv"))


# optional: save a compact summary for logs
elasmos_summary <- elasmos %>%
  summarise(
    n_rows = n(),
    n_trips = n_distinct(trip_id),
    year_min = min(year, na.rm = TRUE),
    year_max = max(year, na.rm = TRUE)
  )

write_csv(elasmos_summary, file.path(summ_dir, "elasmos_summary.csv"))
