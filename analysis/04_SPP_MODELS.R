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

nrow(validated_sightings)

validated_sightings %>% count(validation)

validated_sightings %>%
  filter(validation == "valid") %>%
  summarise(n_valid = n())

validated_sightings %>% summarise(n_trips = n_distinct(url))

# trip-level dataset
nrow(trip_dat)
dplyr::n_distinct(trip_dat$trip_id)
trip_dat %>% summarise(n_regions = n_distinct(region))

# aggregation tables
nrow(trip_group_dat)
nrow(trip_species_dat)

trip_species_dat %>%
  summarise(n_taxa = n_distinct(scientific_name))


print(sp_summary, n=Inf)
sp_summary %>% count(trend)
elasmos %>%
  count(scientific_name, sort = TRUE) %>%
  slice_head(n = 10)


#####  rare and/or CR species ###### 
trip_species_dat %>%
  filter(scientific_name %in% c(
    "Rhinocodon typus",
    "Mobula birostris",
    "Rhynchobatus australiae",
    "Rhynchobatus spp",
    "Rhina anclyostomus",
    "Galeocerdo cuvier",
    "Carcharhinus leucas"
  )) %>%
  count(scientific_name, sort = TRUE)

library(tidyverse)
library(stringr)

# helper: harmonise scientific names across outputs
sci_key <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("\\.", " ") %>%   # convert brms dots to spaces
    str_squish() %>%                  # collapse repeated spaces
    str_to_lower()                    # consistent case
}

sp_summary <- read_csv(file.path(output_dir, "spp-specific", "scientific_name_trends_summary.csv")) %>%
  mutate(sci_key = sci_key(scientific_name))

spp_counts <- trip_species_dat %>%
  group_by(scientific_name) %>%
  summarise(N_individuals = sum(n_species), .groups = "drop") %>%
  mutate(sci_key = sci_key(scientific_name))

spp_iucn <- elasmos_iucn %>%
  distinct(scientific_name, iucn_status) %>%
  mutate(sci_key = sci_key(scientific_name))

table_threatened <- sp_summary %>%
  left_join(spp_counts %>% select(sci_key, N_individuals), by = "sci_key") %>%
  left_join(spp_iucn %>% select(sci_key, iucn_status), by = "sci_key") %>%
  filter(iucn_status %in% c("CR", "EN")) %>%
  mutate(
    direction = case_when(
      p_pos >= 0.90 ~ "Increasing",
      p_pos <= 0.10 ~ "Decreasing",
      TRUE ~ "Uncertain"
    ),
    annual_change = sprintf(
      "%.1f%% (%.1f–%.1f)",
      mean_percent_change,
      l95_percent_change,
      u95_percent_change
    ),
    support_increase = sprintf("%.1f%%", p_pos * 100)
  ) %>%
  transmute(
    scientific_name = str_to_sentence(sci_key),  # nice label for table
    iucn_status,
    N_individuals,
    annual_change,
    direction,
    support_increase
  ) %>%
  arrange(desc(iucn_status), desc(N_individuals))

table_threatened
write_csv(table_threatened, file.path(tables_dir, "table_CR_EN_trends.csv"))

table_threatened_pub <- table_threatened %>%
  mutate(
    posterior_support = case_when(
      direction == "Increasing" ~ support_increase,
      direction == "Decreasing" ~ sprintf("%.1f%%", 100 - as.numeric(str_remove(support_increase, "%"))),
      TRUE ~ support_increase
    ),
    posterior_support = if_else(
      direction == "Uncertain",
      paste0(support_increase, " (increase)"),
      posterior_support
    )
  ) %>%
  select(
    scientific_name,
    iucn_status,
    N_individuals,
    annual_change,
    direction,
    posterior_support
  )

table_threatened_pub
write_csv(table_threatened_pub, file.path(tables_dir, "table_CR_EN_trends_pub.csv"))

# check how many "giant guitarfish" 
trip_species_dat %>%
  filter(scientific_name == "Rhynchobatus spp" ) %>%
  summarise(N = sum(n_species))

trip_species_dat %>%
  filter(scientific_name %in% c(
    "Rhynchobatus australiae",
    "Rhynchobatus spp",
    "Rhina ancylostomus",
    "Rhinocodon typus",
    "Mobula birostris",
    "Galeocerdo cuvier",
    "Carcharhinus leucas",
    "Rhina a"
  )) %>%
  group_by(scientific_name) %>%
  summarise(N = sum(n_species), .groups = "drop") %>%
  arrange(desc(N))

# check 
encounters <- elasmos %>%
  count(scientific_name, name = "n_encounters")

individuals <- trip_species_dat %>%
  group_by(scientific_name) %>%
  summarise(n_individuals = sum(n_species), .groups = "drop")

totals <- left_join(encounters, individuals, by = "scientific_name") %>%
  arrange(desc(n_encounters))

print(totals, n=Inf)

# total encounter records
elasmos %>%
  summarise(total_records = n())

# total individuals
trip_species_dat %>%
  summarise(total_individuals = sum(n_species, na.rm = TRUE))

# number of taxa
trip_species_dat %>%
  summarise(n_taxa = n_distinct(scientific_name))
