##### 05_PLOTS.R ###########

library(tidyverse)
library(brms)
library(tidybayes)

# fig 1 will be a map of sightings 

### Figure 2: Overall temporal trends by country with partial pooling and month held constant ###### 
m_core <- readRDS(file.path(fits_dir, "brms", "m_core.rds"))

# prediction grid
year_seq <- seq(min(trip_dat$year_c), max(trip_dat$year_c), length.out = 200)

newdat_core <- expand_grid(
  year_c = year_seq,
  country = levels(trip_dat$country),
  month = 6,                    # fixed month for interpretability
  region = NA                   # marginalise over random effects
)

core_epred <- m_core %>%
  add_epred_draws(newdata = newdat_core, re_formula = NA) %>%
  group_by(country, year_c) %>%
  summarise(
    mu = mean(.epred),
    l95 = quantile(.epred, 0.025),
    u95 = quantile(.epred, 0.975),
    .groups = "drop"
  )

# core_epred has columns: country, year_c, mu, l95, u95

epred_endpoints <- core_epred %>%
  group_by(country) %>%
  summarise(
    year_c_start = min(year_c),
    mu_start     = mu[which.min(year_c)],
    l95_start    = l95[which.min(year_c)],
    u95_start    = u95[which.min(year_c)],
    year_c_end   = max(year_c),
    mu_end       = mu[which.max(year_c)],
    l95_end      = l95[which.max(year_c)],
    u95_end      = u95[which.max(year_c)],
    .groups = "drop"
  )

epred_endpoints %>%
  mutate(
    year_start = year_c_start + 2012,
    year_end   = year_c_end + 2012
  )
epred_endpoints

p_core_trend <- ggplot(core_epred, aes(x = year_c, y = mu)) +
  geom_ribbon(aes(ymin = l95, ymax = u95), alpha = 0.25) +
  geom_line(linewidth = 1) +
  labs(
    x = "Centered year",
    y = "Expected encounters per trip",
    title = "Trip-level elasmobranch encounters through time",
    subtitle = "Posterior expected counts with 95% credible intervals"
  )

ggsave(file.path(plots_dir, "fig3_core_country_trends.png"),
       p_core_trend, width = 7.2, height = 4.5, dpi = 300, bg = "white")

##### Figure 3: Raw data + model trend overlay for transparency ##### 
p_core_overlay <- ggplot() +
  geom_point(
    data = trip_dat,
    aes(x = year_c, y = n_trip, shape = country),
    alpha = 0.35,
    position = position_jitter(width = 0.08, height = 0)
  ) +
  geom_ribbon(
    data = core_epred,
    aes(x = year_c, ymin = l95, ymax = u95, fill = country),
    alpha = 0.20
  ) +
  geom_line(
    data = core_epred,
    aes(x = year_c, y = mu, color = country),
    linewidth = 1
  ) +
  labs(
    x = "Centered year",
    y = "Encounters per trip (observed and fitted)",
    title = "Observed encounter counts and fitted trends by country"
  )

ggsave(file.path(plots_dir, "fig3_core_overlay.png"),
       p_core_overlay, width = 7.2, height = 4.8, dpi = 300, bg = "white")

##### Figure 4: Sharks vs rays decomposition with country-adjusted trends #######
m_group <- readRDS(file.path(fits_dir, "brms", "m_group.rds"))

year_seq <- seq(min(trip_group_dat$year_c), max(trip_group_dat$year_c), length.out = 200)

newdat_group <- expand_grid(
  year_c = year_seq,
  group = levels(trip_group_dat$group),       # ray, shark
  country = levels(trip_group_dat$country),   # Myanmar, Thailand
  month = 6,
  region = NA,
  trip_id = NA
)

group_epred <- m_group %>%
  add_epred_draws(newdata = newdat_group, re_formula = NA) %>%
  group_by(country, group, year_c) %>%
  summarise(
    mu = mean(.epred),
    l95 = quantile(.epred, 0.025),
    u95 = quantile(.epred, 0.975),
    .groups = "drop"
  )

group_endpoints <- group_epred %>%
  group_by(country, group) %>%
  summarise(
    year_c_start = min(year_c),
    mu_start     = mu[which.min(year_c)],
    l95_start    = l95[which.min(year_c)],
    u95_start    = u95[which.min(year_c)],
    year_c_end   = max(year_c),
    mu_end       = mu[which.max(year_c)],
    l95_end      = l95[which.max(year_c)],
    u95_end      = u95[which.max(year_c)],
    .groups = "drop"
  )  %>%
  mutate(
    year_start = year_c_start + 2012,
    year_end   = year_c_end + 2012
  )

group_endpoints

p_group <- ggplot(group_epred, aes(x = year_c, y = mu)) +
  geom_ribbon(aes(ymin = l95, ymax = u95), alpha = 0.20) +
  geom_line(linewidth = 1) +
  facet_wrap(~ country, nrow = 1) +
  labs(
    x = "Centered year",
    y = "Expected encounters per trip-group",
    title = "Group-specific encounter trajectories",
    subtitle = "Rays (reference) and sharks, adjusted for country and month"
  )

ggsave(file.path(plots_dir, "fig4_group_trends_by_country.png"),
       p_group, width = 7.6, height = 4.2, dpi = 300, bg = "white")

#### Figure 5: Species-level slope caterpillar, but plot percent change with trend classes #### 
sp_summary <- read_csv(file.path(output_dir, "spp-specific", "scientific_name_trends_summary.csv"))

p_spp_pub <- ggplot(
  sp_summary,
  aes(x = reorder(scientific_name, mean_percent_change), y = mean_percent_change)
) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = l95_percent_change, ymax = u95_percent_change), width = 0) +
  geom_point(aes(shape = trend), size = 2) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Estimated annual change in encounters (%)",
    title = "Species-level temporal trajectories",
    subtitle = "Posterior mean and 95% credible interval; slopes pooled across countries"
  ) + theme_clean

ggsave(file.path(plots_dir, "fig5_species_percent_change_pub.png"),
       p_spp_pub, width = 7.8, height = 8.8, dpi = 300, bg = "white")



### print all 
p_core_trend # fig 2 
p_core_overlay # fig 3 
p_group # fig 4 
p_spp_pub # fig 5 


