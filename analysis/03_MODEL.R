### 03_MODEL.R ####
### Purpose: Bayesian models (brms) for core trend + decomposition

### 00. SETUP ####

library(tidyverse)
library(brms)
library(cmdstanr)
library(posterior)
library(bayesplot)

options(mc.cores = parallel::detectCores())
set.seed(42)

# expects from 00_RUN.R:
# - analysis_dir, fits_dir, plots_dir, stats_dir, tables_dir, summ_dir
stopifnot(exists("analysis_dir"))
stopifnot(dir.exists(analysis_dir))

data_dir <- file.path(analysis_dir, "data_clean")
stopifnot(dir.exists(data_dir))


# keep model outputs inside this analysis folder
model_dir <- file.path(fits_dir, "brms")
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)

### 01. LOAD MODELLING DATA ####

trip_dat         <- readRDS(file.path(data_dir, "trip_dat.rds"))
trip_group_dat   <- readRDS(file.path(data_dir, "trip_group_dat.rds"))
trip_species_dat <- readRDS(file.path(data_dir, "trip_species_dat.rds"))

### 02. MODEL SETTINGS ####

options(brms.backend = "cmdstanr")

priors_nb <- c(
  prior(normal(0, 1), class = "b"),
  prior(normal(0, 1.5), class = "Intercept"),
  prior(exponential(1), class = "sd"),
  prior(exponential(1), class = "shape")
)

ctrl <- list(adapt_delta = 0.95, max_treedepth = 12)

### 03. CORE MODEL (TRIP-LEVEL) ####

m_core <- brm(
  n_trip ~ year_c * country + factor(month) + (1 | region),
  data = trip_dat,
  family = negbinomial(link = "log"),
  prior = priors_nb,
  chains = 4, iter = 4000, warmup = 1000,
  control = ctrl,
  backend = "cmdstanr",
  seed = 123,
  file = file.path(model_dir, "m_core")
)
summary(m_core)
# save human-readable summaries
capture.output(summary(m_core), file = file.path(stats_dir, "brms_m_core_summary.txt"))

# posterior predictive check plot
p_ppc_core <- pp_check(m_core, ndraws = 200) +
  ggtitle("brms m_core: PPC (counts)")
ggsave(file.path(plots_dir, "brms_m_core_ppc.png"), p_ppc_core, width = 8, height = 5, dpi = 300)

# derived quantities + probabilities
post_core <- as_draws_df(m_core) %>%
  mutate(
    slope_myan = b_year_c,
    slope_thai = b_year_c + `b_year_c:countryThailand`,
    mult_myan  = exp(slope_myan),
    mult_thai  = exp(slope_thai)
  )

core_summ <- tibble(
  quantity = c(
    "P(slope>0) Myanmar",
    "P(slope>0) Thailand",
    "Mean annual multiplier Myanmar",
    "Mean annual multiplier Thailand"
  ),
  value = c(
    mean(post_core$slope_myan > 0),
    mean(post_core$slope_thai > 0),
    mean(post_core$mult_myan),
    mean(post_core$mult_thai)
  )
)
write_csv(core_summ, file.path(tables_dir, "brms_m_core_prob_increase.csv"))

# save fit object
saveRDS(m_core, file.path(model_dir, "m_core.rds"))


### 04. DECOMPOSITION 1: SHARKS VS RAYS ####

# make reference levels explicit for interpretation
trip_group_dat <- trip_group_dat %>%
  mutate(
    group = relevel(factor(group), ref = "ray"),
    country = relevel(factor(country), ref = "Myanmar")
  )

m_group <- brm(
  n_group ~ year_c * group + country + factor(month) + (1 | region) + (1 | trip_id),
  data = trip_group_dat,
  family = negbinomial(link = "log"),
  prior = priors_nb,
  chains = 4, iter = 4000, warmup = 1000,
  control = ctrl,
  backend = "cmdstanr",
  seed = 123,
  file = file.path(model_dir, "m_group")
)
summary(m_group)
capture.output(summary(m_group), file = file.path(stats_dir, "brms_m_group_summary.txt"))

p_ppc_group <- pp_check(m_group, ndraws = 200) +
  ggtitle("brms m_group: PPC (counts)")
ggsave(file.path(plots_dir, "brms_m_group_ppc.png"), p_ppc_group, width = 8, height = 5, dpi = 300)

post_group <- as_draws_df(m_group)

prob_ray_increase <- mean(post_group$b_year_c > 0)
prob_shark_increase <- mean((post_group$b_year_c + post_group$`b_year_c:groupshark`) > 0)

write_csv(
  tibble(
    group = c("ray (ref)", "shark"),
    prob_slope_gt_0 = c(prob_ray_increase, prob_shark_increase)
  ),
  file.path(tables_dir, "brms_m_group_prob_increase.csv")
)

saveRDS(m_group, file.path(model_dir, "m_group.rds"))

### 05. DECOMPOSITION 2: SPECIES-LEVEL HIERARCHICAL MODEL ####
trip_species_dat <- trip_species_dat %>%
  mutate(scientific_name = recode(scientific_name,
                                  "Acroteriobatus spp" = "Rhinobatidae spp"
  )) %>%
  group_by(across(-n_species)) %>%
  summarise(n_species = sum(n_species), .groups = "drop") %>%
  mutate(
    scientific_name = factor(scientific_name),
    country = relevel(factor(country), ref = "Myanmar")
  )

elasmos <- elasmos %>%
  mutate(scientific_name = recode(scientific_name,
                                  "Acroteriobatus spp" = "Rhinobatidae spp"
  ))

trip_species_dat %>%
  filter(scientific_name == "Rhinobatidae spp") %>%
  summarise(records = n(), individuals = sum(n_species))

m_species <- brm(
  n_species ~ year_c + country + factor(month) +
    (1 + year_c | scientific_name) +
    (1 | trip_id),
  data = trip_species_dat,
  family = negbinomial(link = "log"),
  prior = priors_nb,
  chains = 4, iter = 6000, warmup = 1500,
  control = list(adapt_delta = 0.97, max_treedepth = 13),
  # backend = "cmdstanr",
  seed = 123,
  file = file.path(model_dir, "m_species")
)

summary(m_species)
capture.output(summary(m_species),
               file = file.path(stats_dir, "brms_m_species_summary.txt"))

p_ppc_species <- pp_check(m_species, ndraws = 200) +
  ggtitle("brms m_species: PPC (counts)")
ggsave(file.path(plots_dir, "brms_m_species_ppc.png"),
       p_ppc_species, width = 8, height = 5, dpi = 300)

# scientific_name-specific slope summaries from posterior draws
draws <- as_draws_df(m_species)

spp_slope_cols <- grep("^r_scientific_name\\[.*,year_c\\]$", names(draws), value = TRUE)

spp_slopes <- purrr::map_dfr(spp_slope_cols, function(col) {
  spp <- sub("^r_scientific_name\\[(.*),year_c\\]$", "\\1", col)
  
  slope <- draws$b_year_c + draws[[col]]  # species-specific (pooled across countries)
  
  tibble(
    scientific_name = spp,
    slope_mean = mean(slope),
    slope_q2.5 = quantile(slope, 0.025),
    slope_q97.5 = quantile(slope, 0.975),
    prob_gt0 = mean(slope > 0),
    annual_multiplier_mean = mean(exp(slope)),
    percent_change_mean = (annual_multiplier_mean - 1) * 100
  )
}) %>%
  arrange(desc(prob_gt0))


write_csv(spp_slopes, file.path(tables_dir, "brms_m_species_species_slopes.csv"))

saveRDS(m_species, file.path(model_dir, "m_species.rds"))


summary(m_core)
summary(m_group)
summary(m_species)


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
  ~scientific_name,              ~iucn_status, ~iucn_link,                                                                 ~criteria,                                                                     NA,
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
  "Rhina ancylostomus",          "CR",         "https://www.iucnredlist.org/species/41848/124421912",                     "A2bd",
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
