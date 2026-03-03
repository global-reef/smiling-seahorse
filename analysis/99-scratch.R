library(tidyverse)

# build trip-level aggregation 

trip_dat <- elasmos %>%
  group_by(trip_id) %>%
  summarise(
    year    = first(year),
    month   = first(month),
    ym      = first(ym),
    country = first(country),
    region  = first(region),
    
    # optional: if you want a dominant/most common site_type label per trip
    # site_type = names(sort(table(site_type), decreasing = TRUE))[1],
    
    n_trip  = sum(n_indiv, na.rm = TRUE),
    n_events = n(),                 # number of validated sighting events in that trip
    .groups = "drop"
  )

# quick sanity checks
nrow(trip_dat)                 # should be number of trips (expect ~190)
any(is.na(trip_dat$n_trip))    # should be FALSE
summary(trip_dat$n_trip)

# dispersion checks 
mean(trip_dat$n_trip)
var(trip_dat$n_trip)
max(trip_dat$n_trip)


# plot to explore shape over time 

# Trip-level points over time
# Yearly mean with confidence intervals
# Possibly a LOESS smooth (descriptive only)
library(ggplot2)

# both countries 
ggplot(trip_dat, aes(x = year, y = n_trip)) +
  geom_point(alpha = 0.4) +
  stat_summary(fun = mean, geom = "line", colour = "black", size = 1) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  theme_classic() + geom_smooth(method = "loess", se = TRUE)

#myanmar only 
ggplot(filter(trip_dat, country == "Myanmar"),
       aes(x = year, y = n_trip)) +
  geom_point(alpha = 0.4) +
  stat_summary(fun = mean, geom = "line", colour = "black", size = 1) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  geom_smooth(method = "loess", se = TRUE, colour = "blue") +
  theme_classic()

# check how many trips per region inmyanmar 
trip_dat %>%
  filter(country == "Myanmar") %>%
  count(region)


# testing time modelling # 
myan_dat <- trip_dat %>%
  filter(country == "Myanmar")

library(glmmTMB)

m_lin <- glmmTMB(
  n_trip ~ scale(year) + factor(month) + (1|region),
  family = nbinom2,
  data = myan_dat
)

m_quad <- glmmTMB(
  n_trip ~ scale(year) + I(scale(year)^2) + factor(month) + (1|region),
  family = nbinom2,
  data = myan_dat
)
AIC(m_lin, m_quad)
library(mgcv)

m_gam <- gam(
  n_trip ~ s(year, k = 5) + factor(month),
  family = nb(),
  data = myan_dat
)
AIC(m_lin, m_quad, m_gam)
summary(m_gam)


# check seasonality 
trip_dat %>%
  group_by(year) %>%
  summarise(mean_month = mean(month),
            min_month = min(month),
            max_month = max(month),
            n = n())
sort(unique(trip_dat$month))


## visual check of month effects 
library(ggplot2)

ggplot(myan_dat, aes(x = factor(month), y = n_trip)) +
  geom_boxplot() +
  theme_classic()

# mean by month 
myan_dat %>%
  group_by(month) %>%
  summarise(
    mean_n = mean(n_trip),
    sd_n = sd(n_trip),
    n = n()
  )
# do months shift over years 
myan_dat %>%
  group_by(year) %>%
  summarise(mean_month = mean(month))
plot(myan_dat$year, myan_dat$month)

# taxonimic drivers datasets
trip_group_dat <- elasmos %>%
  group_by(trip_id, year, month, country, region, group) %>%
  summarise(n_group = sum(n_indiv), .groups = "drop")

# allows for n_group ~ scale(year) * group + factor(month) + (1|region)

# species models 
trip_species_dat <- elasmos %>%
  group_by(trip_id, year, month, country, region, species) %>%
  summarise(n_species = sum(n_indiv), .groups = "drop")

# allows for n_species ~ scale(year) + (1 + scale(year) | species) + (1|trip_id)
 
# set ref levels for modelling 
trip_dat <- trip_dat %>%
  mutate(
    country = relevel(country, ref = "Myanmar"),
    year_c  = year - 2012
  )

# frequentist model 
library(glmmTMB)

m_core <- glmmTMB(
  n_trip ~ year_c * country + factor(month) + (1 | region),
  family = nbinom2,
  data = trip_dat
)
summary(m_core)

# Strong evidence of increasing conditional encounter abundance in Myanmar (~17% per year).
# Thailand shows a similar increasing pattern (~10% per year).
# No strong evidence of divergence between countries.
# Increase is not explained by seasonal timing shifts.
# Spatial baseline differences minimal after country accounted for.

# diagnositics 
library(DHARMa)

sim_res <- simulateResiduals(m_core)

plot(sim_res)

testDispersion(sim_res)
testZeroInflation(sim_res)
testOutliers(sim_res)

## all good 




### eco groups model (brms) ####

library(tidyverse)
library(tibble)
library(brms)

# expects: analysis_dir, data_clean_dir, elasmos, priors_nb, ctrl, model_dir, stats_dir
stopifnot(exists("analysis_dir"), exists("elasmos"))
stopifnot(exists("priors_nb"), exists("ctrl"), exists("model_dir"), exists("stats_dir"))

data_clean_dir <- file.path(analysis_dir, "data_clean")
dir.create(data_clean_dir, showWarnings = FALSE, recursive = TRUE)

### eco group lookup ####

eco_lookup <- tribble(
  ~scientific_name,            ~eco_group,               ~note,
  "Rhynchobatus australiae",   "Wedgefish_guitarfish",   NA,
  "Rhynchobatus spp",          "Wedgefish_guitarfish",   "group-level",
  "Rhina anclyostomus",        "Wedgefish_guitarfish",   NA,
  "Acroteriobatus spp",        "Wedgefish_guitarfish",   "group-level",
  "Rhinobatidae spp",          "Wedgefish_guitarfish",   "family/group-level",
  
  "Mobula alfredii",           "Mobulids",               NA,
  "Mobula birostris",          "Mobulids",               NA,
  "Mobula spp",                "Mobulids",               "group-level",
  
  "Rhinocodon typus",          "Large_pelagic",          NA,
  "Galeocerdo cuvier",         "Large_pelagic",          NA,
  
  "Triaenodon obesus",         "Reef_sharks",            NA,
  "Carcharhinus melanopterus", "Reef_sharks",            NA,
  "Carcharhinus amblyrhynchos","Reef_sharks",            "mobile reef-associated",
  
  "Carcharhinus leucas",       "Large_coastal_sharks",   NA,
  "Carcharhinus limbatus",     "Large_coastal_sharks",   NA,
  "Carcharhinus albimarginatus","Large_coastal_sharks",  NA,
  
  "Chiloscyllium punctatum",   "Resident_or_benthic",    NA,
  "Neotrygon kuhlii",          "Resident_or_benthic",    NA,
  "Pastinachus ater",          "Resident_or_benthic",    NA,
  "Taeniurops meyeni",         "Resident_or_benthic",    NA,
  "Urogymnus asperrimus",      "Resident_or_benthic",    NA,
  "Urogymnus granulatus",      "Resident_or_benthic",    NA,
  "Pateobatis jenkinsii",      "Resident_or_benthic",    NA,
  "Aetobatus ocellatus",       "Resident_or_benthic",    "arguable mobility",
  "Batidae spp",               "Resident_or_benthic",    "group-level",
  
  "Selachii spp",              "Unresolved_group_level", "group-level",
  "Carcharhinidae spp",        "Unresolved_group_level", "family/group-level",
  
  # fill your 3 missing assignments (edit if you want different bins)
  "Atelomycterus marmoratus",  "Resident_or_benthic",    NA,
  "Nebrius ferrugineus",       "Resident_or_benthic",    NA,
  "Stegostoma tigrinum",       "Resident_or_benthic",    NA
)

# optional: write draft to inspect
elasmos %>%
  distinct(scientific_name) %>%
  mutate(scientific_name = as.character(scientific_name)) %>%
  left_join(eco_lookup, by = "scientific_name") %>%
  arrange(scientific_name) %>%
  write_csv(file.path(data_clean_dir, "eco_group_lookup.csv"))

### join eco_group onto elasmos ####

elasmos_ecogroup <- elasmos %>%
  mutate(scientific_name = as.character(scientific_name)) %>%
  left_join(eco_lookup, by = "scientific_name") %>%
  mutate(eco_group = factor(eco_group))

# hard stop if anything still missing
missing_groups <- elasmos_ecogroup %>%
  distinct(scientific_name, eco_group) %>%
  filter(is.na(eco_group)) %>%
  pull(scientific_name)

if (length(missing_groups) > 0) {
  stop("Eco group missing for: ", paste(missing_groups, collapse = ", "))
}

write_csv(elasmos_ecogroup, file.path(data_clean_dir, "elasmos_sightings_ecogroup.csv"))
saveRDS(elasmos_ecogroup, file.path(data_clean_dir, "elasmos_sightings_ecogroup.rds"))

### build trip-level eco_group dataset ####

trip_ecogroup_dat <- elasmos_ecogroup %>%
  group_by(trip_id, year, country, region, eco_group) %>%
  summarise(n_group = sum(n_indiv, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    year_c = year - 2012,
    country = relevel(factor(country), ref = "Myanmar")
  )

### fit model (NEW FILE NAME) ####

m_ecogroup <- brm(
  n_group ~ year_c * eco_group + country + (1 | region) + (1 | trip_id),
  data = trip_ecogroup_dat,
  family = negbinomial(link = "log"),
  prior = priors_nb,
  chains = 4, iter = 4000, warmup = 1000,
  control = ctrl,
  backend = "cmdstanr",
  seed = 123,
  file = file.path(model_dir, "m_ecogroup")
)

capture.output(summary(m_ecogroup), file = file.path(stats_dir, "brms_m_ecogroup_summary.txt"))

summary(m_ecogroup)



