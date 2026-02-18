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
# - fits_dir, plots_dir, stats_dir, tables_dir, summ_dir
data_dir <- "data_clean"

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
  mutate(
    scientific_name = factor(scientific_name),
    country = relevel(factor(country), ref = "Myanmar")
  )

m_species <- brm(
  n_species ~ year_c + country + factor(month) +
    (1 + year_c | scientific_name) +
    (1 | trip_id),
  data = trip_species_dat,
  family = negbinomial(link = "log"),
  prior = priors_nb,
  chains = 4, iter = 6000, warmup = 1500,
  control = list(adapt_delta = 0.97, max_treedepth = 13),
  backend = "cmdstanr",
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
