##### 05_PLOTS.R ###########

library(tidyverse)
library(brms)
library(tidybayes)

# fig 1 will be a map of sightings 
# see 06_MAP.R 

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

### Figure 5: species-level percent-change caterpillar by group

library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(tibble)

### Load species trend summary

sp_summary <- read_csv(file.path(output_dir, "spp-specific", "scientific_name_trends_summary.csv"))

### Species metadata from IUCN-linked records

elasmos_iucn <- elasmos_iucn |>
  mutate(group = if_else(scientific_name %in% c("Rhynchobatus australiae", "Rhynchobatus spp"),
                         "ray", group))

spp_meta <- elasmos_iucn |>
  mutate(name_join = scientific_name |>
           str_replace_all("\\.", " ") |>
           str_squish() |>
           str_to_lower()) |>
  distinct(name_join, scientific_name, group, iucn_status, iucn_threatened, iucn_severity) |>
  group_by(name_join, group) |>
  slice_max(iucn_severity, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(
    panel = case_when(
      group == "shark" ~ "Sharks",
      group == "ray" ~ "Rays and batoids",
      TRUE ~ NA_character_
    ),
    focal_threatened = iucn_status %in% c("CR", "EN")
  )

### Colours

trend_cols <- c(
  "Increasing (> 0)" = "#007A87",
  "Uncertain (CI overlaps 0)" = "grey50",
  "Decreasing (< 0)" = "#D55E00"
)

### Prepare plot data

sp_summary_plot <- sp_summary |>
  mutate(
    scientific_name_clean = scientific_name |> str_replace_all("\\.", " "),
    name_join = scientific_name_clean |> str_squish() |> str_to_lower()
  ) |>
  left_join(spp_meta, by = "name_join", suffix = c("", "_iucn")) |>
  filter(panel %in% c("Sharks", "Rays and batoids")) |>
  mutate(
    panel = factor(panel, levels = c("Sharks", "Rays and batoids")),
    trend_class = case_when(
      l95_percent_change > 0 ~ "Increasing (> 0)",
      u95_percent_change < 0 ~ "Decreasing (< 0)",
      TRUE ~ "Uncertain (CI overlaps 0)"
    ),
    trend_class = factor(trend_class, levels = names(trend_cols)),
    star = if_else(focal_threatened, "~\"*\"", ""),
    label_plotmath = case_when(
      str_detect(scientific_name_clean, " spp$") ~ paste0(
        "italic(",
        str_replace_all(str_remove(scientific_name_clean, " spp$"), " ", "~"),
        ")~spp",
        star
      ),
      TRUE ~ paste0(
        "italic(",
        str_replace_all(scientific_name_clean, " ", "~"),
        ")",
        star
      )
    )
  ) |>
  group_by(panel) |>
  arrange(desc(mean_percent_change), .by_group = TRUE) |>
  mutate(
    label_key = paste(panel, scientific_name_clean, row_number(), sep = "___"),
    label_order = factor(label_key, levels = rev(label_key))
  ) |>
  ungroup()

### Label lookup for parsed italic labels

label_lookup <- sp_summary_plot |>
  distinct(label_order, label_plotmath) |>
  deframe()

### Sanity checks

sp_summary_plot |> count(panel, trend_class)

sp_summary |>
  mutate(name_join = scientific_name |>
           str_replace_all("\\.", " ") |>
           str_squish() |>
           str_to_lower()) |>
  anti_join(spp_meta, by = "name_join") |>
  distinct(scientific_name)

### Plot

p_spp_pub <- ggplot(
  sp_summary_plot,
  aes(y = label_order, x = mean_percent_change, colour = trend_class)
) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey45", linewidth = 0.35) +
  geom_segment(aes(x = l95_percent_change, xend = u95_percent_change, yend = label_order),
               linewidth = 0.55, alpha = 0.9) +
  geom_point(size = 2.1) +
  facet_wrap(
    ~panel, ncol = 1, scales = "free_y",
    labeller = as_labeller(c(
      "Sharks" = "A",
      "Rays and batoids" = "B"
    ))
  ) +
  scale_y_discrete(labels = \(x) parse(text = label_lookup[x])) +
  scale_colour_manual(values = trend_cols, name = NULL, drop = FALSE) +
  labs(
    x = "Estimated annual change in encounters (% per year)",
    y = NULL
  ) +
  theme_clean +
  theme(
    text = element_text(family = "sans"),
    panel.grid = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(size = 8.5, colour = "black"),
    axis.text.x = element_text(size = 9, colour = "black"),
    axis.title.x = element_text(size = 10, colour = "black"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", hjust = 0, size = 11),
    legend.position = "bottom",
    legend.text = element_text(size = 8.5),
    panel.spacing.y = unit(0.8, "lines")
  )

p_spp_pub

### Export

ggsave(file.path(plots_dir, "fig5_species_percent_change_pub.png"),
       p_spp_pub, width = 7.8, height = 8.8, dpi = 300, bg = "white")

ggsave(file.path(plots_dir, "fig5_species_percent_change_pub.pdf"),
       p_spp_pub, width = 7.8, height = 8.8, device = cairo_pdf, bg = "white")


### print all 
p_core_trend # fig 2 
p_core_overlay # fig 3 
p_group # fig 4 
p_spp_pub # fig 5 


