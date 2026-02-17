### 00. SETUP ####

library(tidyverse)
library(ggplot2)
library(glmmTMB)
library(mgcv)
library(DHARMa)

### 01. BUILD TRIP-LEVEL DATASETS ####

# --- trip-level aggregation (primary modelling unit) ---
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
    # modelling-friendly coding
    country = relevel(country, ref = "Myanmar"),
    year_c  = year - 2012
  )

# --- Myanmar-only convenience object for EDA / model shape checks ---
myan_dat <- trip_dat %>%
  filter(country == "Myanmar")

# --- taxonomic driver datasets (trip × group, trip × species) ---
trip_group_dat <- elasmos %>%
  group_by(trip_id, year, month, country, region, group) %>%
  summarise(n_group = sum(n_indiv, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    country = relevel(country, ref = "Myanmar"),
    year_c  = year - 2012
  )

trip_species_dat <- elasmos %>%
  group_by(trip_id, year, month, country, region, species) %>%
  summarise(n_species = sum(n_indiv, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    country = relevel(country, ref = "Myanmar"),
    year_c  = year - 2012
  )

### 02. SANITY CHECKS ####

nrow(trip_dat)                      # expected ~190 trips
any(is.na(trip_dat$n_trip))         # should be FALSE
summary(trip_dat$n_trip)

# dispersion / tail checks
mean(trip_dat$n_trip)
var(trip_dat$n_trip)
max(trip_dat$n_trip)

# confirm months sampled
sort(unique(trip_dat$month))

### 03. ZUUR-STYLE EDA ####

## 03A. OUTLIERS IN RESPONSE (Y) ####

# simple boxplot of n_trip
ggplot(trip_dat, aes(x = "", y = n_trip)) +
  geom_boxplot() +
  theme_classic() +
  labs(x = NULL, y = "Total individuals per trip (n_trip)")

# identify top 5 extreme trips
trip_dat %>%
  arrange(desc(n_trip)) %>%
  slice(1:5) %>%
  select(trip_id, year, month, country, region, n_trip, n_events)

## 03B. ZERO STRUCTURE (PRESENCE-CONDITIONED) ####

# confirm: no zero-trips in dataset by design
sum(trip_dat$n_trip == 0)

## 03C. RELATIONSHIP BETWEEN Y AND TIME (SHAPE CHECKS) ####

# both countries: points + yearly mean + LOESS
ggplot(trip_dat, aes(x = year, y = n_trip)) +
  geom_point(alpha = 0.4) +
  stat_summary(fun = mean, geom = "line", colour = "black", size = 1) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  geom_smooth(method = "loess", se = TRUE) +
  theme_classic() +
  labs(y = "n_trip")

# Myanmar only: points + yearly mean + LOESS
ggplot(myan_dat, aes(x = year, y = n_trip)) +
  geom_point(alpha = 0.4) +
  stat_summary(fun = mean, geom = "line", colour = "black", size = 1) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  geom_smooth(method = "loess", se = TRUE, colour = "blue") +
  theme_classic() +
  labs(y = "n_trip (Myanmar)")

# log-scale visual (helps interpret proportional change)
ggplot(trip_dat, aes(year, n_trip)) +
  geom_point(alpha = 0.4) +
  scale_y_log10() +
  facet_wrap(~country) +
  theme_classic() +
  labs(y = "n_trip (log10 scale)")

# capped visual to see whether curve is dominated by extreme events
ggplot(trip_dat, aes(year, pmin(n_trip, 30))) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = TRUE) +
  facet_wrap(~country) +
  theme_classic() +
  labs(y = "n_trip capped at 30 (visual only)")

## 03D. REGION REPLICATION (MYANMAR) ####

trip_dat %>%
  filter(country == "Myanmar") %>%
  count(region)

## 03E. TIME-STRUCTURE TESTING (LINEAR VS NONLINEAR) ####

# linear NB GLMM (Myanmar only)
m_lin <- glmmTMB(
  n_trip ~ scale(year) + factor(month) + (1 | region),
  family = nbinom2,
  data = myan_dat
)

# quadratic NB GLMM (Myanmar only)
m_quad <- glmmTMB(
  n_trip ~ scale(year) + I(scale(year)^2) + factor(month) + (1 | region),
  family = nbinom2,
  data = myan_dat
)

# GAM NB (Myanmar only) - used for shape exploration
m_gam <- gam(
  n_trip ~ s(year, k = 5) + factor(month),
  family = nb(),
  data = myan_dat
)

AIC(m_lin, m_quad, m_gam)
summary(m_gam)

## 03F. SEASONALITY CHECKS ####

# do months shift over years (all data)
trip_dat %>%
  group_by(year) %>%
  summarise(
    mean_month = mean(month),
    min_month  = min(month),
    max_month  = max(month),
    n = n(),
    .groups = "drop"
  )

# Myanmar month effect: boxplot
ggplot(myan_dat, aes(x = factor(month), y = n_trip)) +
  geom_boxplot() +
  theme_classic() +
  labs(x = "month", y = "n_trip (Myanmar)")

# Myanmar month summary
myan_dat %>%
  group_by(month) %>%
  summarise(
    mean_n = mean(n_trip),
    sd_n   = sd(n_trip),
    n      = n(),
    .groups = "drop"
  )

# month drift by year (Myanmar)
myan_dat %>%
  group_by(year) %>%
  summarise(mean_month = mean(month), n = n(), .groups = "drop")

plot(myan_dat$year, myan_dat$month)

## 03G. INTERACTION (COUNTRY) QUICK VISUAL ####

ggplot(trip_dat, aes(year, n_trip)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = TRUE) +
  facet_wrap(~country) +
  theme_classic() +
  labs(y = "n_trip")

### 04. CORE MODEL (FREQUENTIST) ####

m_core <- glmmTMB(
  n_trip ~ year_c * country + factor(month) + (1 | region),
  family = nbinom2,
  data = trip_dat
)

summary(m_core)

### 05. DIAGNOSTICS (DHARMa) ####

sim_res <- simulateResiduals(m_core)

plot(sim_res)

testDispersion(sim_res)
testZeroInflation(sim_res)  # expected to flag "zero deficit" due to presence-conditioned dataset
testOutliers(sim_res)

# residual autocorrelation check 
trip_dat_ord <- trip_dat %>% arrange(ym)
res <- residuals(m_core, type = "pearson")
acf(res[order(trip_dat_ord$ym)], na.action = na.pass)
# no meaningful temporal dependence across trips 


### 06. SAVE MODELLING DATASETS ####

saveRDS(trip_dat,         "data_clean/trip_dat.rds")
saveRDS(trip_group_dat,   "data_clean/trip_group_dat.rds")
saveRDS(trip_species_dat, "data_clean/trip_species_dat.rds")


# The data:
# Are overdispersed counts → NB appropriate.
# Show a broadly monotonic increase in Myanmar.
# Do not show strong nonlinear structure requiring GAM as primary.
# Are not temporally autocorrelated.
# Are not seasonally confounded with time.
# Are conditional on presence and interpreted as such.
