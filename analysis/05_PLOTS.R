##### 05_PLOTS.R ###########

library(tidyverse)
library(brms)
library(tidybayes)
library(patchwork)

# fig 1 will be a map of sightings 
# see 06_MAP.R 

### Figure NA: Overall temporal trends ####

m_core <- readRDS(file.path(fits_dir, "brms", "m_core.rds"))

year_seq <- seq(min(trip_dat$year_c), max(trip_dat$year_c), length.out = 200)

newdat_core <- expand_grid(
  year_c = year_seq,
  country = levels(trip_dat$country),
  month = sort(unique(trip_dat$month)),
  region = NA_character_
)

core_epred <- m_core %>%
  add_epred_draws(newdata = newdat_core, re_formula = NA) %>%
  group_by(.draw, country, year_c) %>%
  summarise(.epred = mean(.epred), .groups = "drop") %>%
  group_by(country, year_c) %>%
  summarise(mu = mean(.epred), l95 = quantile(.epred, 0.025),
            u95 = quantile(.epred, 0.975), .groups = "drop") %>%
  mutate(year = year_c + 2012)

epred_endpoints <- core_epred %>%
  group_by(country) %>%
  summarise(
    year_start = min(year), mu_start = mu[which.min(year)],
    l95_start = l95[which.min(year)], u95_start = u95[which.min(year)],
    year_end = max(year), mu_end = mu[which.max(year)],
    l95_end = l95[which.max(year)], u95_end = u95[which.max(year)],
    .groups = "drop"
  )

p_core_trend <- ggplot(core_epred, aes(year, mu, colour = country, fill = country)) +
  geom_ribbon(aes(ymin = l95, ymax = u95), alpha = 0.18, colour = NA) +
  geom_line(linewidth = 1.1) +
  scale_colour_manual(values = country_cols) +
  scale_fill_manual(values = country_cols) +
  scale_x_continuous(breaks = seq(2012, 2025, 2)) +
  labs(x = "Year", y = "Expected elasmobranch encounters per trip", colour = NULL, fill = NULL) +
  theme_clean +
  theme(
    axis.text = element_text(colour = "black"),
    axis.title = element_text(colour = "black"),
    legend.position = "bottom"
  )

p_core_trend

ggsave(file.path(plots_dir, "fig2_core_country_trends.png"),
       p_core_trend, width = 7.2, height = 4.5, dpi = 600, bg = "white")

##### Figure 4: Raw data + model trend overlay for transparency ##### 

p_core_overlay <- ggplot() +
  geom_point(
    data = trip_dat,
    aes(year_c + 2012, n_trip, colour = country, shape = country),
    alpha = 0.25, size = 1.5,
    position = position_jitter(width = 0.08, height = 0)
  ) +
  scale_shape_manual(values = c("Myanmar" = 16, "Thailand" = 23)) +
  geom_ribbon(
    data = core_epred,
    aes(year, ymin = l95, ymax = u95, fill = country),
    alpha = 0.16, colour = NA
  ) +
  geom_line(
    data = core_epred,
    aes(year, mu, colour = country),
    linewidth = 1.1
  ) +
  scale_colour_manual(values = country_cols) +
  scale_fill_manual(values = country_cols) +
  scale_x_continuous(breaks = seq(2012, 2025, 2)) +
  labs(x = "Year", y = "Elasmobranch encounters per trip", colour = NULL, fill = NULL) +
  theme_clean +
  theme(
    axis.text = element_text(colour = "black"),
    axis.title = element_text(colour = "black"),
    legend.position = "bottom"
  ) + guides(shape = "none")

p_core_overlay

ggsave(file.path(plots_dir, "fig4_core_overlay.png"),
       p_core_overlay, width = 4.8, height = 4.8, dpi = 600, bg = "white")

p_core_overlay_pdf <- p_core_overlay +
  theme(text = element_text(family = "sans"))

ggsave(file.path(plots_dir, "fig4_core_overlay.pdf"),
       p_core_overlay_pdf, width = 4.8, height = 4.8, bg = "white")

##### Figure 5: Sharks vs rays decomposition with country-adjusted trends #######

m_group <- readRDS(file.path(fits_dir, "brms", "m_group.rds"))

year_seq <- seq(min(trip_group_dat$year_c), max(trip_group_dat$year_c), length.out = 200)

newdat_group <- expand_grid(
  year_c = year_seq,
  group = levels(trip_group_dat$group),
  country = levels(trip_group_dat$country),
  month = sort(unique(trip_group_dat$month)),
  region = NA_character_,
  trip_id = NA_character_
)

group_epred <- m_group %>%
  add_epred_draws(newdata = newdat_group, re_formula = NA) %>%
  group_by(.draw, country, group, year_c) %>%
  summarise(.epred = mean(.epred), .groups = "drop") %>%
  group_by(country, group, year_c) %>%
  summarise(mu = mean(.epred), l95 = quantile(.epred, 0.025),
            u95 = quantile(.epred, 0.975), .groups = "drop") %>%
  mutate(year = year_c + 2012)

p_group <- ggplot(group_epred, aes(year, mu, colour = group, fill = group)) +
  geom_ribbon(aes(ymin = l95, ymax = u95), alpha = 0.18, colour = NA) +
  geom_line(linewidth = 1.1) +
  facet_wrap(~country, nrow = 1, labeller = as_labeller(c("Myanmar" = "A", "Thailand" = "B"))) +
  scale_colour_manual(values = elasmo_cols, labels = c("ray" = "Rays", "shark" = "Sharks")) +
  scale_fill_manual(values = elasmo_cols, labels = c("ray" = "Rays", "shark" = "Sharks")) +
  scale_x_continuous(breaks = seq(2012, 2025, 4)) +
  labs(x = "Year", y = "Expected encounters per trip", colour = NULL, fill = NULL) +
  theme_clean +
  theme(
    axis.text = element_text(colour = "black"), axis.title = element_text(colour = "black"),
    strip.background = element_blank(), strip.text = element_text(face = "bold", hjust = 0, size = 11),
    legend.position = "bottom", panel.spacing.x = unit(1, "lines")
  )

p_group

ggsave(file.path(plots_dir, "fig5_group_trends_by_country.png"),
       p_group, width = 7.6, height = 4.2, dpi = 600, bg = "white")

ggsave(file.path(plots_dir, "fig5_group_trends_by_country.pdf"),
       p_group + theme(text = element_text(family = "sans")),
       width = 7.6, height = 4.2, bg = "white")


##### Figure NA: Species-specific caterpillar #######

library(readr)
library(stringr)
library(tibble)
library(ggplot2)
library(dplyr)

#### Load species trend summary 

sp_summary <- read_csv(file.path(output_dir, "spp-specific", "scientific_name_trends_summary.csv")) %>%
  mutate(scientific_name = scientific_name %>%
           str_replace_all("\\.", " ") %>%
           recode("Neotrygon kuhlii" = "Neotrygon caeruleopunctata"))

## Species metadata from IUCN-linked records 

spp_meta <- elasmos_iucn %>%
  mutate(
    scientific_name = recode(scientific_name, "Neotrygon kuhlii" = "Neotrygon caeruleopunctata"),
    group = if_else(scientific_name %in% c("Rhynchobatus australiae", "Rhynchobatus spp"), "ray", group),
    name_join = scientific_name %>% str_replace_all("\\.", " ") %>% str_squish() %>% str_to_lower()
  ) %>%
  distinct(name_join, scientific_name, group, iucn_status, iucn_threatened, iucn_severity) %>%
  group_by(name_join, group) %>%
  slice_max(iucn_severity, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    panel = case_when(group == "shark" ~ "Sharks", group == "ray" ~ "Rays and batoids", TRUE ~ NA_character_),
    focal_threatened = iucn_status %in% c("CR", "EN")
  )

#### Colours 

trend_cols <- c(
  "Increasing" = "#007A87",
  "Uncertain" = "grey50",
  "Decreasing" = "#D55E00"
)


#### Prepare plot data 

sp_summary_plot <- sp_summary %>%
  mutate(
    scientific_name_clean = scientific_name %>% str_replace_all("\\.", " "),
    name_join = scientific_name_clean %>% str_squish() %>% str_to_lower()
  ) %>%
  left_join(spp_meta, by = "name_join", suffix = c("", "_iucn")) %>%
  filter(panel %in% c("Sharks", "Rays and batoids")) %>%
  mutate(
    panel = factor(panel, levels = c("Sharks", "Rays and batoids")),
    trend_class = case_when(
      p_pos > 0.90 ~ "Increasing",
      p_pos < 0.10 ~ "Decreasing",
      TRUE ~ "Uncertain"
    ),
    trend_class = factor(trend_class, levels = names(trend_cols)),
    star = if_else(focal_threatened, "~'*'", ""),
    label_plotmath = case_when(
      str_detect(scientific_name_clean, " spp$") ~ paste0(
        "italic(", str_remove(scientific_name_clean, " spp$") %>% str_replace_all(" ", "~"), ")~spp", star
      ),
      TRUE ~ paste0("italic(", scientific_name_clean %>% str_replace_all(" ", "~"), ")", star)
    )
  ) %>%
  mutate(
    label_key = paste(panel, scientific_name_clean, sep = "__"),
    label_order = reorder(label_key, mean_percent_change)
  )

#### Label lookup 

label_lookup <- sp_summary_plot %>%
  distinct(label_order, label_plotmath) %>%
  deframe()

#### Sanity checks 

sp_summary_plot %>% count(panel, trend_class)

sp_summary %>%
  mutate(name_join = scientific_name %>% str_replace_all("\\.", " ") %>% str_squish() %>% str_to_lower()) %>%
  anti_join(spp_meta, by = "name_join") %>%
  distinct(scientific_name)

#### Plot 

p_spp_pub1 <- ggplot(sp_summary_plot, aes(x = mean_percent_change, y = label_order, colour = trend_class)) +
  geom_point(
    data = tibble(trend_class = factor(names(trend_cols), levels = names(trend_cols))),
    aes(x = Inf, y = NA, colour = trend_class),
    inherit.aes = FALSE, alpha = 0, show.legend = TRUE
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey45", linewidth = 0.35) +
  geom_segment(aes(x = l95_percent_change, xend = u95_percent_change, yend = label_order), linewidth = 0.55, alpha = 0.9) +
  geom_point(size = 2.1) +
  facet_wrap(~panel, ncol = 1, scales = "free_y", labeller = as_labeller(c("Sharks" = "A", "Rays and batoids" = "B"))) +
  scale_y_discrete(labels = \(x) parse(text = label_lookup[x])) +
  scale_colour_manual(values = trend_cols, breaks = names(trend_cols), name = NULL, drop = FALSE) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 2.1))) +
  labs(x = "Estimated annual change in encounters (% per year)", y = NULL) +
  theme_clean +
  theme(
    text = element_text(family = "sans"), panel.grid = element_blank(), axis.ticks.y = element_blank(),
    axis.text.y = element_text(size = 8.5, colour = "black"), axis.text.x = element_text(size = 9, colour = "black"),
    axis.title.x = element_text(size = 10, colour = "black"), strip.background = element_blank(),
    strip.text = element_text(face = "bold", hjust = 0, size = 11), legend.position = "bottom",
    legend.text = element_text(size = 8.5), panel.spacing.y = unit(0.8, "lines")
  )

p_spp_pub1

#### Export 

ggsave(file.path(plots_dir, "fig6i_species_percent_change_pub.png"),
       p_spp_pub1, width = 7, height = 7, dpi = 600, bg = "white")

ggsave(file.path(plots_dir, "fig6i_species_percent_change_pub.pdf"),
       p_spp_pub1, width = 7, height = 7, bg = "white")





### Figure 6: Spp-level posterior distributions ####

#### Load posterior draws + summaries

sp_draws <- readRDS(file.path(output_dir, "spp-specific", "scientific_name_slope_draws.rds")) %>%
  mutate(scientific_name = scientific_name %>%
           str_replace_all("\\.", " ") %>%
           recode(
             "Neotrygon kuhlii" = "Neotrygon caeruleopunctata",
             "Batidae spp" = "Batoidea spp"
           ))

sp_summary <- read_csv(file.path(output_dir, "spp-specific", "scientific_name_trends_summary.csv")) %>%
  mutate(scientific_name = scientific_name %>%
           str_replace_all("\\.", " ") %>%
           recode(
             "Neotrygon kuhlii" = "Neotrygon caeruleopunctata",
             "Batidae spp" = "Batoidea spp"
           ))

#### Species metadata 

spp_meta <- elasmos_iucn %>%
  mutate(
    scientific_name = recode(
      scientific_name,
      "Neotrygon kuhlii" = "Neotrygon caeruleopunctata",
      "Batidae spp" = "Batoidea spp"
    ),
    name_join = scientific_name %>% str_replace_all("\\.", " ") %>% str_squish() %>% str_to_lower()
  ) %>%
  distinct(name_join, scientific_name, group, iucn_status, iucn_threatened, iucn_severity) %>%
  group_by(name_join, group) %>%
  slice_max(iucn_severity, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    panel = case_when(group == "shark" ~ "Sharks", group == "ray" ~ "Rays", TRUE ~ NA_character_),
    focal_threatened = iucn_status %in% c("CR", "EN")
  )

#### Colours 

trend_cols <- c(
  "Increasing" = "#007A87",
  "Uncertain" = "grey50",
  "Decreasing" = "#D55E00"
)

#### Species labels + ordering 

sp_plot_meta <- sp_summary %>%
  mutate(name_join = scientific_name %>% str_squish() %>% str_to_lower()) %>%
  left_join(spp_meta, by = "name_join", suffix = c("", "_iucn")) %>%
  filter(panel %in% c("Sharks", "Rays")) %>%
  mutate(
    panel = factor(panel, levels = c("Sharks", "Rays")),
    trend_class = case_when(
      p_pos > 0.90 ~ "Increasing",
      p_pos < 0.10 ~ "Decreasing",
      TRUE ~ "Uncertain"
    ),
    trend_class = factor(trend_class, levels = names(trend_cols)),
    star = if_else(focal_threatened, "~'*'", ""),
    label_plotmath = case_when(
      str_detect(scientific_name, " spp$") ~ paste0(
        "italic(", str_remove(scientific_name, " spp$") %>% str_replace_all(" ", "~"), ")~spp", star
      ),
      TRUE ~ paste0("italic(", scientific_name %>% str_replace_all(" ", "~"), ")", star)
    ),
    label_key = paste(panel, scientific_name, sep = "__")
  ) %>%
  arrange(mean_percent_change) %>%
  mutate(label_order = factor(label_key, levels = unique(label_key)))

label_lookup <- sp_plot_meta %>%
  distinct(label_order, label_plotmath) %>%
  deframe()

#### Join posterior draws 

sp_draws_plot <- sp_draws %>%
  mutate(name_join = scientific_name %>% str_squish() %>% str_to_lower()) %>%
  left_join(
    sp_plot_meta %>% select(name_join, panel, trend_class, label_key, label_order),
    by = "name_join"
  ) %>%
  filter(!is.na(panel))

#### Plot 

p_spp_pub <- ggplot(
  sp_draws_plot,
  aes(x = percent_change, y = label_order, fill = trend_class, colour = trend_class)
) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey45", linewidth = 0.35) +
  stat_halfeye(.width = 0.95, point_interval = mean_qi, slab_alpha = 0.65,
               point_size = 1.6, interval_size = 0.6) +
  facet_wrap(~panel, ncol = 1, scales = "free_y",
             labeller = as_labeller(c("Sharks" = "A", "Rays" = "B"))) +
  coord_cartesian(xlim = c(-22, 26)) +
  scale_x_continuous(breaks = c(-20, 0, 20)) +
  scale_y_discrete(labels = \(x) parse(text = label_lookup[x])) +
  scale_fill_manual(values = trend_cols, breaks = names(trend_cols), drop = FALSE, name = NULL) +
  scale_colour_manual(values = trend_cols, breaks = names(trend_cols), drop = FALSE, name = NULL) +
  labs(x = "Estimated annual change in encounters (% per year)", y = NULL) +
  theme_clean +
  theme(
    text = element_text(family = "sans"),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(size = 9.5, colour = "black"),
    axis.text.x = element_text(size = 9, colour = "black"),
    axis.title.x = element_text(size = 10, colour = "black"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", hjust = 0, size = 11),
    legend.position = "bottom",
    legend.text = element_text(size = 8.5),
    panel.spacing.y = unit(0.5, "lines")
  )

p_spp_pub



#### Export 

ggsave(file.path(plots_dir, "fig6_species_posterior_distributions.png"),
       p_spp_pub, width = 6.4, height = 6.8, dpi = 600, bg = "white")

ggsave(file.path(plots_dir, "fig6_species_posterior_distributions.pdf"),
       p_spp_pub, width = 6.4, height = 6.8, bg = "white")


### print all 
p_core_trend # fig 4 not using 
p_core_overlay # fig 4
p_group # fig 5
p_spp_pub # fig 6




##### Figure 5: Group trends + posterior slope distributions ####

library(patchwork)

#### Prediction data 

m_group <- readRDS(file.path(fits_dir, "brms", "m_group.rds"))

year_seq <- seq(min(trip_group_dat$year_c), max(trip_group_dat$year_c), length.out = 200)

newdat_group <- expand_grid(
  year_c = year_seq,
  group = levels(trip_group_dat$group),
  country = levels(trip_group_dat$country),
  month = sort(unique(trip_group_dat$month)),
  region = NA_character_,
  trip_id = NA_character_
)

group_epred <- m_group %>%
  add_epred_draws(newdata = newdat_group, re_formula = NA) %>%
  group_by(.draw, country, group, year_c) %>%
  summarise(.epred = mean(.epred), .groups = "drop") %>%
  group_by(country, group, year_c) %>%
  summarise(mu = mean(.epred), l95 = quantile(.epred, 0.025),
            u95 = quantile(.epred, 0.975), .groups = "drop") %>%
  mutate(
    year = year_c + 2012,
    group_lab = recode(group, "shark" = "Sharks", "ray" = "Rays"),
    group_lab = factor(group_lab, levels = c("Sharks", "Rays"))
  )

#### Posterior group slopes 

post_group <- as_draws_df(m_group) %>%
  transmute(
    Rays = (exp(b_year_c) - 1) * 100,
    Sharks = (exp(b_year_c + `b_year_c:groupshark`) - 1) * 100
  ) %>%
  pivot_longer(everything(), names_to = "group_lab", values_to = "percent_change") %>%
  mutate(
    group_lab = factor(group_lab, levels = c("Rays", "Sharks")),
    group = recode(group_lab, "Sharks" = "shark", "Rays" = "ray")
  )

#### A + B: temporal trends by group ####

p_group_trend <- ggplot(group_epred, aes(year, mu, colour = country, fill = country)) +
  geom_ribbon(aes(ymin = l95, ymax = u95), alpha = 0.16, colour = NA) +
  geom_line(linewidth = 1.15) +
  facet_wrap(~group_lab, nrow = 1,
             labeller = as_labeller(c("Sharks" = "A", "Rays" = "B"))) +
  scale_colour_manual(values = country_cols) +
  scale_fill_manual(values = country_cols) +
  scale_x_continuous(breaks = seq(2012, 2025, 4)) +
  labs(x = "Year", y = "Expected encounters per trip", colour = NULL, fill = NULL) +
  theme_clean +
  theme(
    axis.text = element_text(size = 9, colour = "black"),
    axis.title = element_text(size = 10, colour = "black"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", hjust = 0, size = 11),
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    panel.spacing.x = unit(1, "lines")
  )

#### C: posterior annual change ####

p_group_post <- ggplot(post_group, aes(percent_change, group_lab, fill = group, colour = group)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey45", linewidth = 0.35) +
  stat_halfeye(.width = 0.95, point_interval = mean_qi, slab_alpha = 0.7,
               point_size = 1.8, interval_size = 0.9) +
  scale_colour_manual(values = elasmo_cols, guide = "none") +
  scale_fill_manual(values = elasmo_cols, guide = "none") +
  labs(x = "Annual change (% per year)", y = NULL, title = "C") +
  theme_clean +
  theme(
    axis.text = element_text(size = 9, colour = "black"),
    axis.title = element_text(size = 9, colour = "black"),
    axis.ticks.y = element_blank(),
    plot.title = element_text(face = "bold", size = 11, hjust = -0.4, vjust = -6),
    plot.title.position = "panel",
    plot.margin = margin(5.5, 5.5, 5.5, 0)
  )

#### combine ####

p_group <- p_group_trend + p_group_post +
  plot_layout(widths = c(3.8, 1.2), guides = "collect") &
  theme(legend.position = "bottom")

p_group

ggsave(file.path(plots_dir, "fig5i_group_trends_by_group.png"),
       p_group, width = 7.2, height = 4.2, dpi = 600, bg = "white")

ggsave(file.path(plots_dir, "fig5i_group_trends_by_group.pdf"),
       p_group + theme(text = element_text(family = "sans")),
       width = 7.2, height = 4.2, bg = "white")

