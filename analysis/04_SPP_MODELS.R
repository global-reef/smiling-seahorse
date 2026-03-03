### 04_SPP.R ############################################################
# Species-level trend extraction from m_species (grouped by scientific_name)
# Outputs:
# 1) species trend summary table (csv)
# 2) species slope caterpillar plot (png)
# 3) posterior draws (rds)

### setup ####

library(tidyverse)
library(tidybayes)
library(brms)

options(dplyr.summarise.inform = FALSE)

# expects from 00_RUN.R:
# - output_dir, fits_dir
stopifnot(exists("output_dir"))
stopifnot(dir.exists(output_dir))
stopifnot(exists("fits_dir"))
stopifnot(dir.exists(fits_dir))

out_spp <- file.path(output_dir, "spp-specific")
dir.create(out_spp, recursive = TRUE, showWarnings = FALSE)

### load model ####

# if not already in environment, load from disk
if (!exists("m_species")) {
  model_dir <- file.path(fits_dir, "brms")
  m_path <- file.path(model_dir, "m_species.rds")
  stopifnot(file.exists(m_path))
  m_species <- readRDS(m_path)
}

### extract scientific_name-specific year slopes ####
# expected model:
# n_species ~ year_c + country + factor(month) +
#   (1 + year_c | scientific_name) + (1 | trip_id)

sp_draws <- m_species %>%
  spread_draws(r_scientific_name[scientific_name, term], b_year_c) %>%
  filter(term == "year_c") %>%
  mutate(
    total_slope = b_year_c + r_scientific_name,
    annual_multiplier = exp(total_slope),
    percent_change = (annual_multiplier - 1) * 100
  )

### summarise trends per scientific_name ####

sp_summary <- sp_draws %>%
  group_by(scientific_name) %>%
  summarise(
    mean_slope = mean(total_slope),
    l95 = quantile(total_slope, 0.025),
    u95 = quantile(total_slope, 0.975),
    p_pos = mean(total_slope > 0),
    mean_percent_change = mean(percent_change),
    l95_percent_change = quantile(percent_change, 0.025),
    u95_percent_change = quantile(percent_change, 0.975),
    .groups = "drop"
  ) %>%
  mutate(
    trend = case_when(
      p_pos > 0.90 ~ "Increasing",
      p_pos < 0.10 ~ "Decreasing",
      TRUE ~ "Uncertain"
    )
  ) %>%
  arrange(desc(mean_slope))

### save tables ####

write_csv(sp_summary, file.path(out_spp, "scientific_name_trends_summary.csv"))
saveRDS(sp_draws, file.path(out_spp, "scientific_name_slope_draws.rds"))

### plot: caterpillar of scientific_name slopes ####

p_spp_slope <- ggplot(
  sp_summary,
  aes(x = reorder(scientific_name, mean_slope), y = mean_slope)
) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = l95, ymax = u95), width = 0) +
  geom_point() +
  coord_flip() +
  labs(
    x = NULL,
    y = "Scientific-name-specific year slope (log scale)",
    title = "Species-level trends in encounters through time",
    subtitle = "Points are posterior means; bars are 95% credible intervals"
  )

ggsave(
  filename = file.path(out_spp, "fig_scientific_name_slopes.png"),
  plot = p_spp_slope,
  width = 9,
  height = 10,
  dpi = 300
)

### optional: plot in percent change per year ####

p_spp_pct <- ggplot(
  sp_summary,
  aes(x = reorder(scientific_name, mean_percent_change), y = mean_percent_change)
) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = l95_percent_change, ymax = u95_percent_change), width = 0) +
  geom_point() +
  coord_flip() +
  labs(
    x = NULL,
    y = "Estimated percent change per year",
    title = "Species-level annual change (percent scale)",
    subtitle = "Derived from exp(slope); bars are 95% credible intervals"
  )

ggsave(
  filename = file.path(out_spp, "fig_scientific_name_percent_change.png"),
  plot = p_spp_pct,
  width = 9,
  height = 10,
  dpi = 300
)

### quick check: trend counts + top 10 ####

sp_summary %>%
  count(trend)

sp_summary %>%
  slice_head(n = 10) %>%
  select(scientific_name, trend, mean_slope, l95, u95, p_pos, mean_percent_change)

# END BASE MODELS  ####






### IUCN LOOKUP + JOIN + SENSITIVITY (species-slope ~ IUCN severity) ####
### Purpose:
### 1) define IUCN lookup for taxa in `elasmos`
### 2) join onto `elasmos` and save
### 3) sensitivity: does IUCN severity predict species-specific year slopes from `sp_draws`?

#### 00. SETUP ####

library(tidyverse)
library(tibble)

# expects from 00_RUN.R / earlier scripts
stopifnot(exists("analysis_dir"))
stopifnot(exists("elasmos"))
stopifnot(exists("output_dir"))  # used to find `sp_draws` output

data_clean_dir <- file.path(analysis_dir, "data_clean")
dir.create(data_clean_dir, showWarnings = FALSE, recursive = TRUE)

#### 01. IUCN LOOKUP TABLE ####

iucn_lookup <- tribble(
  ~scientific_name,              ~iucn_status, ~iucn_link,                                                                 ~criteria,
  "Acroteriobatus spp",          NA,           NA,                                                                         NA,
  "Aetobatus ocellatus",         "EN",         "https://www.iucnredlist.org/search?query=Aetobatus%20ocellatus&searchType=species", "A2bcd",
  "Atelomycterus marmoratus",    "NT",         "https://www.iucnredlist.org/species/41730/124414963",                     "A2cd",
  "Batidae spp",                 NA,           NA,                                                                         NA,
  "Carcharhinidae spp",          NA,           NA,                                                                         NA,
  "Carcharhinus albimarginatus", "VU",         "https://www.iucnredlist.org/species/161526/124499982",                    "A2bd",
  "Carcharhinus amblyrhynchos",  "EN",         "https://www.iucnredlist.org/species/39365/173433550",                     "A2bcd",
  "Carcharhinus leucas",         "VU",         "https://www.iucnredlist.org/species/39372/2910670",                       "A2bcd",
  "Carcharhinus limbatus",       "VU",         "https://www.iucnredlist.org/species/3851/2870736",                        "A2bd",
  "Carcharhinus melanopterus",   "VU",         "https://www.iucnredlist.org/species/39375/58303674",                      "A2bcd",
  "Chiloscyllium punctatum",     "NT",         "https://www.iucnredlist.org/species/41872/124423551",                     "A2d",
  "Galeocerdo cuvier",           "NT",         "https://www.iucnredlist.org/species/39378/2913541",                       "A2bd+3d",
  "Mobula alfredii",             "VU",         "https://www.iucnredlist.org/species/195459/214395983",                    "A2bcd+3d",
  "Mobula birostris",            "EN",         "https://www.iucnredlist.org/species/198921/214397182",                    "A2bcd+3d",
  "Mobula spp",                  NA,           NA,                                                                         NA,
  "Nebrius ferrugineus",         "VU",         "https://www.iucnredlist.org/species/41835/173437098",                     "A2bcd",
  "Neotrygon kuhlii",            "DD",         "https://www.iucnredlist.org/species/116847578/116849874",                 NA,
  "Pastinachus ater",            "VU",         "https://www.iucnredlist.org/species/70682232/124550583",                  "A2d",
  "Pateobatis jenkinsii",        "EN",         "https://www.iucnredlist.org/species/161744/124536951",                    "A2cd",
  "Rhina anclyostomus",          "CR",         "https://www.iucnredlist.org/species/41848/124421912",                     "A2bd",
  "Rhinobatidae spp",            NA,           NA,                                                                         NA,
  "Rhinocodon typus",            "EN",         "https://www.iucnredlist.org/species/19488/126673248",                     "A2bd",
  "Rhynchobatus australiae",     "CR",         "https://www.iucnredlist.org/species/41853/68643043",                      "A2bd",
  "Rhynchobatus spp",            NA,           NA,                                                                         NA,
  "Selachii spp",                NA,           NA,                                                                         NA,
  "Stegostoma tigrinum",         "EN",         "https://www.iucnredlist.org/species/41878/124425292",                     "A2bcd",
  "Taeniurops meyeni",           "VU",         "https://www.iucnredlist.org/species/60162/124445924",                     "A2cd",
  "Triaenodon obesus",           "VU",         "https://www.iucnredlist.org/species/39384/173436715",                     "A2bcd",
  "Urogymnus asperrimus",        "EN",         "https://www.iucnredlist.org/species/39413/124411670",                     "A2cd",
  "Urogymnus granulatus",        "EN",         "https://www.iucnredlist.org/species/161431/124484009",                    "A2cd"
)

stopifnot(!anyDuplicated(iucn_lookup$scientific_name))

# optional: save a draft CSV ordered like your data
iucn_lookup_out <- elasmos %>%
  distinct(scientific_name) %>%
  mutate(scientific_name = as.character(scientific_name)) %>%
  arrange(scientific_name) %>%
  left_join(iucn_lookup, by = "scientific_name")

write_csv(iucn_lookup_out, file.path(data_clean_dir, "iucn_lookup_draft.csv"))

#### 02. JOIN IUCN ONTO ELASMOS + SAVE ####

missing_iucn <- setdiff(
  sort(unique(as.character(elasmos$scientific_name))),
  iucn_lookup$scientific_name
)
if (length(missing_iucn) > 0) {
  stop("IUCN lookup missing scientific_name values: ", paste(missing_iucn, collapse = ", "))
}

elasmos_iucn <- elasmos %>%
  mutate(scientific_name = as.character(scientific_name)) %>%
  left_join(iucn_lookup, by = "scientific_name") %>%
  mutate(
    iucn_threatened = case_when(
      iucn_status %in% c("CR", "EN", "VU") ~ "Threatened",
      iucn_status %in% c("NT", "LC")       ~ "Not_threatened",
      iucn_status == "DD"                  ~ "Data_deficient",
      TRUE                                 ~ NA_character_
    ),
    iucn_threatened = factor(iucn_threatened, levels = c("Not_threatened", "Threatened", "Data_deficient")),
    iucn_severity = case_when(
      iucn_status == "NT" ~ 1,
      iucn_status == "VU" ~ 2,
      iucn_status == "EN" ~ 3,
      iucn_status == "CR" ~ 4,
      TRUE ~ NA_real_
    )
  )

write_csv(elasmos_iucn, file.path(data_clean_dir, "elasmos_sightings_iucn.csv"))
saveRDS(elasmos_iucn, file.path(data_clean_dir, "elasmos_sightings_iucn.rds"))

#### 03. SENSITIVITY: DO SPECIES YEAR-SLOPES VARY WITH IUCN SEVERITY? ####
### Uses posterior slope draws already saved by your species-slope extraction script.

out_spp <- file.path(output_dir, "spp-specific")
sp_path <- file.path(out_spp, "scientific_name_slope_draws.rds")
stopifnot(file.exists(sp_path))

sp_draws <- readRDS(sp_path)

# helper to harmonise names (brms random-effect labels often replace spaces with dots)
norm_sciname <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("\\.", " ") %>%
    str_squish() %>%
    str_to_sentence()
}

spp_iucn <- elasmos_iucn %>%
  distinct(scientific_name, iucn_status, iucn_severity) %>%
  filter(!is.na(iucn_severity)) %>%
  mutate(scientific_name = norm_sciname(scientific_name))

sp_draws_iucn <- sp_draws %>%
  mutate(scientific_name = norm_sciname(scientific_name)) %>%
  left_join(spp_iucn, by = "scientific_name")

# compute per-draw slope of (total_slope ~ iucn_severity), then summarise across draws
slope_test <- sp_draws_iucn %>%
  filter(!is.na(iucn_severity)) %>%
  group_by(.draw) %>%
  summarise(
    beta = coef(lm(total_slope ~ iucn_severity))[2],
    .groups = "drop"
  ) %>%
  summarise(
    mean_beta = mean(beta),
    l95 = quantile(beta, 0.025),
    u95 = quantile(beta, 0.975),
    p_pos = mean(beta > 0)
  )

write_csv(slope_test, file.path(out_spp, "iucn_severity_slope_sensitivity.csv"))
slope_test

#### end #### 
