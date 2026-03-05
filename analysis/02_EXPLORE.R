### 02_EXPLORE.R ####
### Purpose: Zuur-style EDA + build modelling datasets; save datasets for modelling

### 00. SETUP ####

library(tidyverse)
library(ggplot2)
library(glmmTMB)
library(mgcv)
library(DHARMa)

# expects these from 00_RUN.R:
# - eda_dir, plots_dir, stats_dir, summ_dir (etc.)
# expects: analysis_dir from 00_RUN.R
stopifnot(exists("analysis_dir"))
stopifnot(dir.exists(analysis_dir))

data_clean_dir <- file.path(analysis_dir, "data_clean")
dir.create(data_clean_dir, showWarnings = FALSE, recursive = TRUE)


# small helpers for saving plots + tables without cluttering env
save_plot <- function(p, filename, w = 7, h = 5, dir = eda_dir) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  ggsave(filename = file.path(dir, filename), plot = p, width = w, height = h, dpi = 300)
  invisible(TRUE)
}

save_tbl <- function(x, filename, dir = eda_dir) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  write_csv(x, file.path(dir, filename))
  invisible(TRUE)
}

### 01. LOAD CLEAN DATA ####

elasmos <- read_csv(file.path(data_clean_dir, "elasmos_sightings.csv"), show_col_types = FALSE)


### 02. BUILD MODELLING DATASETS ####

trip_dat <- elasmos %>%
  group_by(trip_id) %>%
  summarise(
    year    = first(year),
    month   = first(month),
    ym      = first(ym),
    country = first(country),
    region  = first(region),
    n_trip  = sum(n_indiv, na.rm = TRUE),
    n_events = n(),
    .groups = "drop"
  ) %>%
  mutate(
    country = relevel(factor(country), ref = "Myanmar"),
    year_c  = year - 2012
  )

myan_dat <- trip_dat %>%
  filter(country == "Myanmar")

trip_group_dat <- elasmos %>%
  group_by(trip_id, year, month, country, region, group) %>%
  summarise(n_group = sum(n_indiv, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    country = relevel(factor(country), ref = "Myanmar"),
    year_c  = year - 2012
  )

trip_species_dat <- elasmos %>%
  group_by(trip_id, year, month, country, region, species, scientific_name) %>%
  summarise(n_species = sum(n_indiv, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    country = relevel(factor(country), ref = "Myanmar"),
    year_c  = year - 2012
  )

### 03. SANITY CHECKS ####

sanity_summary <- trip_dat %>%
  summarise(
    n_trips = n(),
    any_na_n_trip = any(is.na(n_trip)),
    mean_n = mean(n_trip),
    var_n  = var(n_trip),
    max_n  = max(n_trip),
    months = paste(sort(unique(month)), collapse = ", ")
  )

save_tbl(sanity_summary, "sanity_trip_dat.csv", dir = summ_dir)

### 04. ZUUR-STYLE EDA ####

## 04A. OUTLIERS IN RESPONSE (Y) ####

p_box <- ggplot(trip_dat, aes(x = "", y = n_trip)) +
  geom_boxplot() +
  theme_classic() +
  labs(x = NULL, y = "Total individuals per trip (n_trip)")

save_plot(p_box, "01_boxplot_n_trip.png")

top_trips <- trip_dat %>%
  arrange(desc(n_trip)) %>%
  slice(1:10)

save_tbl(top_trips, "02_top10_trips_by_n_trip.csv")

## 04B. ZERO STRUCTURE (PRESENCE-CONDITIONED) ####

zero_check <- tibble(n_zero_trips = sum(trip_dat$n_trip == 0))
save_tbl(zero_check, "03_zero_trip_check.csv")

## 04C. Y VS TIME (SHAPE) ####

p_all <- ggplot(trip_dat, aes(x = year, y = n_trip)) +
  geom_point(alpha = 0.4) +
  stat_summary(fun = mean, geom = "line", colour = "black", size = 1) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  geom_smooth(method = "loess", se = TRUE) +
  theme_classic() +
  labs(y = "n_trip")

save_plot(p_all, "04_time_all_year_mean_loess.png", w = 8, h = 5)

p_myan <- ggplot(myan_dat, aes(x = year, y = n_trip)) +
  geom_point(alpha = 0.4) +
  stat_summary(fun = mean, geom = "line", colour = "black", size = 1) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  geom_smooth(method = "loess", se = TRUE, colour = "blue") +
  theme_classic() +
  labs(y = "n_trip (Myanmar)")

save_plot(p_myan, "05_time_myanmar_year_mean_loess.png", w = 8, h = 5)

p_log <- ggplot(trip_dat, aes(year, n_trip)) +
  geom_point(alpha = 0.4) +
  scale_y_log10() +
  facet_wrap(~country) +
  theme_classic() +
  labs(y = "n_trip (log10 scale)")

save_plot(p_log, "06_time_log10_facet_country.png", w = 8, h = 5)

p_cap <- ggplot(trip_dat, aes(year, pmin(n_trip, 30))) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = TRUE, colour = "blue") +
  facet_wrap(~country) +
  theme_classic() +
  labs(y = "n_trip capped at 30 (visual only)")

save_plot(p_cap, "07_time_capped30_loess_facet_country.png", w = 8, h = 5)

## 04D. REGION REPLICATION ####

region_counts_myan <- trip_dat %>%
  filter(country == "Myanmar") %>%
  count(region)

save_tbl(region_counts_myan, "08_region_counts_myanmar.csv")

## 04E. TIME-STRUCTURE TESTING (MYANMAR) ####

m_lin <- glmmTMB(
  n_trip ~ scale(year) + factor(month) + (1 | region),
  family = nbinom2,
  data = myan_dat
)

m_quad <- glmmTMB(
  n_trip ~ scale(year) + I(scale(year)^2) + factor(month) + (1 | region),
  family = nbinom2,
  data = myan_dat
)

m_gam <- gam(
  n_trip ~ s(year, k = 5) + factor(month),
  family = nb(),
  data = myan_dat
)

aic_shape <- AIC(m_lin, m_quad, m_gam) %>%
  as.data.frame() %>%
  rownames_to_column("model")

save_tbl(aic_shape, "09_time_shape_aic_myanmar.csv")

# keep GAM summary as a text artifact (lightweight)
capture.output(summary(m_gam), file = file.path(stats_dir, "10_m_gam_summary.txt"))

## 04F. SEASONALITY CHECKS ####

month_by_year <- trip_dat %>%
  group_by(year) %>%
  summarise(
    mean_month = mean(month),
    min_month  = min(month),
    max_month  = max(month),
    n = n(),
    .groups = "drop"
  )

save_tbl(month_by_year, "11_month_by_year_all.csv")

p_month_box <- ggplot(myan_dat, aes(x = factor(month), y = n_trip)) +
  geom_boxplot() +
  theme_classic() +
  labs(x = "month", y = "n_trip (Myanmar)")

save_plot(p_month_box, "12_month_boxplot_myanmar.png", w = 7, h = 5)

month_summary_myan <- myan_dat %>%
  group_by(month) %>%
  summarise(
    mean_n = mean(n_trip),
    sd_n   = sd(n_trip),
    n      = n(),
    .groups = "drop"
  )

save_tbl(month_summary_myan, "13_month_summary_myanmar.csv")

month_drift_myan <- myan_dat %>%
  group_by(year) %>%
  summarise(mean_month = mean(month), n = n(), .groups = "drop")

save_tbl(month_drift_myan, "14_month_drift_myanmar.csv")

## 04G. INTERACTION VISUAL ####

p_int <- ggplot(trip_dat, aes(year, n_trip)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = TRUE) +
  facet_wrap(~country) +
  theme_classic() +
  labs(y = "n_trip")

save_plot(p_int, "15_country_loess_facet.png", w = 8, h = 5)

### 05. CORE MODEL (FREQUENTIST) + DIAGNOSTICS ####

m_core <- glmmTMB(
  n_trip ~ year_c * country + factor(month) + (1 | region),
  family = nbinom2,
  data = trip_dat
)

capture.output(summary(m_core), file = file.path(stats_dir, "16_m_core_summary.txt"))

sim_res <- simulateResiduals(m_core)
# save DHARMa plot
png(file.path(stats_dir, "17_DHARMa_m_core.png"), width = 1200, height = 700, res = 150)
plot(sim_res)
dev.off()

disp_test <- testDispersion(sim_res)
zi_test   <- testZeroInflation(sim_res)
out_test  <- testOutliers(sim_res)

# write tests to text
capture.output(disp_test, file = file.path(stats_dir, "18_DHARMa_dispersion.txt"))
capture.output(zi_test,   file = file.path(stats_dir, "19_DHARMa_zero_inflation.txt"))
capture.output(out_test,  file = file.path(stats_dir, "20_DHARMa_outliers.txt"))

# residual autocorrelation
trip_dat_ord <- trip_dat %>% arrange(ym)
res <- residuals(m_core, type = "pearson")

png(file.path(stats_dir, "21_residual_acf.png"), width = 900, height = 700, res = 150)
acf(res[order(trip_dat_ord$ym)], na.action = na.pass)
dev.off()

### 06. SAVE MODELLING DATASETS ####
saveRDS(trip_dat,         file.path(data_clean_dir, "trip_dat.rds"))
saveRDS(trip_group_dat,   file.path(data_clean_dir, "trip_group_dat.rds"))
saveRDS(trip_species_dat, file.path(data_clean_dir, "trip_species_dat.rds"))



#### some numbers for the main methods #### 
range(trip_dat$ym, na.rm = TRUE)
## trips 
trip_dat %>%
  summarise(
    year_min = min(year, na.rm = TRUE),
    year_max = max(year, na.rm = TRUE),
    n_trips  = n_distinct(trip_id)
  )

trip_dat %>% summarise(n_trips = n_distinct(trip_id))
nrow(trip_dat)

trip_group_dat %>%
  summarise(
    n_rows = n(),
    n_trips = n_distinct(trip_id),
    rows_per_trip = n_rows / n_trips
  )

trip_group_dat %>% count(trip_id) %>% count(n, name = "n_trips")
### species 
trip_species_dat %>%
  summarise(
    n_rows = n(),
    n_trips = n_distinct(trip_id),
    n_species = n_distinct(scientific_name)
  )

# if elasmos exists (sightings-level)
nrow(validated_sightings)

validated_sightings %>% count(validation)  # if validation column still present
## regions 
trip_dat %>%
  distinct(country, region) %>%
  count(country, name = "n_regions")

trip_dat %>% distinct(region) %>% summarise(n_regions_total = n())

trip_dat %>%
  count(country, region, name = "n_trips") %>%
  arrange(country, desc(n_trips))

trip_dat %>%
  distinct(country, region) %>%
  filter(str_detect(str_to_lower(region), "mergui|burma|banks"))

trips_by_year <- trip_dat %>%
  count(year, country, name = "n_trips") %>%
  arrange(year, country)

print(trips_by_year, n=Inf)

trip_dat %>%
  group_by(country) %>%
  summarise(
    year_min = min(year, na.rm = TRUE),
    year_max = max(year, na.rm = TRUE),
    n_trips = n_distinct(trip_id),
    mean_trips_per_year = n_trips / (year_max - year_min + 1)
  )

# Requires sightings-level data with trip_id + dive_site_std (or dive_site)
trip_itinerary <- elasmos %>%
  distinct(trip_id, dive_site) %>%
  count(trip_id, name = "n_sites")

trip_itinerary %>%
  summarise(
    mean_sites = mean(n_sites),
    median_sites = median(n_sites),
    min_sites = min(n_sites),
    max_sites = max(n_sites)
  )

trip_itinerary %>% count(n_sites) %>% arrange(n_sites)


