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

# end ####
