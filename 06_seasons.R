### 06_SITE_SEASON_MODELS.R ####
### Purpose: explore species-specific encounter probability by dive site and season
### Output: summary tables + predicted site-season encounter probabilities for common taxa

### 00. SETUP ####

library(tidyverse)
library(glmmTMB)
library(broom.mixed)

# expects from 00_RUN.R:
# - analysis_dir, output_dir
stopifnot(exists("analysis_dir"))
stopifnot(dir.exists(analysis_dir))
stopifnot(exists("output_dir"))
stopifnot(dir.exists(output_dir))

data_clean_dir <- file.path(analysis_dir, "data_clean")
stopifnot(dir.exists(data_clean_dir))

site_season_dir <- file.path(output_dir, "site-season")
dir.create(site_season_dir, recursive = TRUE, showWarnings = FALSE)

### 01. LOAD CLEAN DATA ####

elasmos <- read_csv(
  file.path(data_clean_dir, "elasmos_sightings.csv"),
  show_col_types = FALSE
)

### 02. BASIC CLEANING ####

elasmos <- elasmos %>%
  mutate(
    trip_id = as.character(trip_id),
    scientific_name = as.character(scientific_name),
    dive_site = as.character(dive_site),
    country = as.character(country),
    region = as.character(region),
    year = as.integer(year),
    month = as.integer(month),
    n_indiv = as.numeric(n_indiv)
  ) %>%
  filter(
    !is.na(trip_id),
    !is.na(scientific_name),
    !is.na(dive_site),
    !is.na(country),
    !is.na(region),
    !is.na(year),
    !is.na(month)
  ) %>%
  mutate(
    year_c = year - 2012,
    season = case_when(
      month %in% c(11, 12, 1) ~ "early",
      month %in% c(2, 3) ~ "mid",
      month %in% c(4, 5) ~ "late",
      TRUE ~ "other"
    ),
    season = factor(season, levels = c("early", "mid", "late", "other"))
  )

### 03. BUILD TRIP-SITE TABLE ####
### one row per trip x visited site

trip_site <- elasmos %>%
  distinct(trip_id, dive_site, country, region, year, month, year_c, season) %>%
  mutate(visited = 1)

write_csv(trip_site, file.path(site_season_dir, "trip_site.csv"))

### 04. BUILD TRIP-SITE-SPECIES DETECTION TABLE ####
### one row per trip x site x species
### detected = 1 if species recorded there on that trip, else 0

species_list <- elasmos %>%
  distinct(scientific_name)

detections <- elasmos %>%
  group_by(trip_id, dive_site, scientific_name) %>%
  summarise(
    n_indiv_site_species = sum(n_indiv, na.rm = TRUE),
    detected = 1L,
    .groups = "drop"
  )

trip_site_species <- trip_site %>%
  tidyr::crossing(species_list) %>%
  left_join(
    detections,
    by = c("trip_id", "dive_site", "scientific_name")
  ) %>%
  mutate(
    detected = if_else(is.na(detected), 0L, detected),
    n_indiv_site_species = replace_na(n_indiv_site_species, 0)
  )

write_csv(
  trip_site_species,
  file.path(site_season_dir, "trip_site_species_detection.csv")
)

### 05. FILTER TO COMMON ENOUGH SPECIES ####

min_detections <- 15
min_sites <- 3
min_years <- 3

species_screen <- trip_site_species %>%
  group_by(scientific_name) %>%
  summarise(
    n_rows = n(),
    n_detect = sum(detected),
    n_sites_detected = n_distinct(dive_site[detected == 1]),
    n_trips_detected = n_distinct(trip_id[detected == 1]),
    n_years_detected = n_distinct(year[detected == 1]),
    .groups = "drop"
  ) %>%
  arrange(desc(n_detect))

write_csv(
  species_screen,
  file.path(site_season_dir, "species_screening_summary.csv")
)

species_keep <- species_screen %>%
  filter(
    n_detect >= min_detections,
    n_sites_detected >= min_sites,
    n_years_detected >= min_years
  ) %>%
  pull(scientific_name)

### 06. HELPER FUNCTION ####

fit_one_species <- function(sp_name,
                            dat,
                            min_site_n = 5) {
  
  dat_sp <- dat %>%
    filter(scientific_name == sp_name)
  
  # keep only sites with enough trip-site rows
  site_keep <- dat_sp %>%
    count(dive_site, name = "n_site_rows") %>%
    filter(n_site_rows >= min_site_n) %>%
    pull(dive_site)
  
  dat_sp <- dat_sp %>%
    filter(dive_site %in% site_keep)
  
  # must still have variation in detections
  if (nrow(dat_sp) == 0) {
    return(NULL)
  }
  
  if (length(unique(dat_sp$detected)) < 2) {
    return(NULL)
  }
  
  if (n_distinct(dat_sp$dive_site) < 2) {
    return(NULL)
  }
  
  if (n_distinct(dat_sp$season[dat_sp$detected == 1]) < 2) {
    message("Sparse seasonal spread for: ", sp_name)
  }
  
  dat_sp <- dat_sp %>%
    mutate(
      dive_site = factor(dive_site),
      season = droplevels(factor(season, levels = c("early", "mid", "late", "other")))
    )
  
  # remove unused season levels if absent
  dat_sp <- dat_sp %>%
    filter(!is.na(season))
  
  if (n_distinct(dat_sp$season) < 2) {
    return(
      list(
        species = sp_name,
        data = dat_sp,
        fit = NULL,
        coef_tbl = NULL,
        pred_tbl = NULL,
        top_tbl = NULL,
        model_formula = "detected ~ year_c + season + dive_site + (1 | trip_id)",
        converged = FALSE,
        note = "Only one season level remained after filtering"
      )
    )
  }
  
  model_formula <- detected ~ year_c + season + dive_site + (1 | trip_id)
  
  fit <- try(
    glmmTMB(
      formula = model_formula,
      data = dat_sp,
      family = binomial(link = "logit")
    ),
    silent = TRUE
  )
  
  if (inherits(fit, "try-error")) {
    return(
      list(
        species = sp_name,
        data = dat_sp,
        fit = NULL,
        coef_tbl = NULL,
        pred_tbl = NULL,
        top_tbl = NULL,
        model_formula = deparse(model_formula),
        converged = FALSE,
        note = "Model fit failed"
      )
    )
  }
  
  # check convergence more carefully
  fit_ok <- TRUE
  fit_note <- "OK"
  
  if (!is.null(fit$sdr$pdHess) && !isTRUE(fit$sdr$pdHess)) {
    fit_ok <- FALSE
    fit_note <- "Non-positive-definite Hessian"
  }
  
  if (!is.null(fit$fit$convergence) && fit$fit$convergence != 0) {
    fit_ok <- FALSE
    fit_note <- paste("Optimizer convergence code:", fit$fit$convergence)
  }
  
  coef_tbl <- try(
    broom.mixed::tidy(fit, effects = "fixed") %>%
      mutate(
        scientific_name = sp_name,
        odds_ratio = exp(estimate),
        l95_or = exp(estimate - 1.96 * std.error),
        u95_or = exp(estimate + 1.96 * std.error)
      ) %>%
      select(
        scientific_name, term, estimate, std.error, statistic, p.value,
        odds_ratio, l95_or, u95_or
      ),
    silent = TRUE
  )
  
  if (inherits(coef_tbl, "try-error")) {
    coef_tbl <- NULL
  }
  
  pred_grid <- expand_grid(
    season = levels(dat_sp$season),
    dive_site = levels(dat_sp$dive_site)
  ) %>%
    mutate(
      year_c = round(mean(dat_sp$year_c, na.rm = TRUE)),
      trip_id = NA
    )
  
  pred <- try(
    predict(
      fit,
      newdata = pred_grid,
      type = "link",
      se.fit = TRUE,
      re.form = NA
    ),
    silent = TRUE
  )
  
  if (inherits(pred, "try-error")) {
    return(
      list(
        species = sp_name,
        data = dat_sp,
        fit = fit,
        coef_tbl = coef_tbl,
        pred_tbl = NULL,
        top_tbl = NULL,
        model_formula = deparse(model_formula),
        converged = fit_ok,
        note = paste(fit_note, "| prediction failed")
      )
    )
  }
  
  pred_tbl <- pred_grid %>%
    mutate(
      scientific_name = sp_name,
      eta = pred$fit,
      se = pred$se.fit,
      prob = plogis(eta),
      l95 = plogis(eta - 1.96 * se),
      u95 = plogis(eta + 1.96 * se)
    ) %>%
    select(scientific_name, season, dive_site, year_c, prob, l95, u95, eta, se)
  
  top_tbl <- pred_tbl %>%
    arrange(desc(prob)) %>%
    mutate(rank = row_number()) %>%
    select(scientific_name, rank, dive_site, season, prob, l95, u95) %>%
    slice_head(n = 15)
  
  list(
    species = sp_name,
    data = dat_sp,
    fit = fit,
    coef_tbl = coef_tbl,
    pred_tbl = pred_tbl,
    top_tbl = top_tbl,
    model_formula = deparse(model_formula),
    converged = fit_ok,
    note = fit_note
  )
}

### 07. FIT MODELS ####

results_list <- purrr::map(
  species_keep,
  ~fit_one_species(
    sp_name = .x,
    dat = trip_site_species,
    min_site_n = 5
  )
)

names(results_list) <- species_keep

### 08. EXTRACT OUTPUTS ####

model_summary <- purrr::map_dfr(results_list, function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  
  tibble(
    scientific_name = x$species,
    converged = x$converged,
    note = x$note,
    formula = x$model_formula,
    n_rows = ifelse(is.null(x$data), NA_integer_, nrow(x$data)),
    n_detect = ifelse(is.null(x$data), NA_integer_, sum(x$data$detected)),
    n_sites = ifelse(is.null(x$data), NA_integer_, n_distinct(x$data$dive_site)),
    n_seasons = ifelse(is.null(x$data), NA_integer_, n_distinct(x$data$season)),
    n_years = ifelse(is.null(x$data), NA_integer_, n_distinct(x$data$year))
  )
})

coef_all <- purrr::map_dfr(results_list, ~.x$coef_tbl)
pred_all <- purrr::map_dfr(results_list, ~.x$pred_tbl)
top_all  <- purrr::map_dfr(results_list, ~.x$top_tbl)

write_csv(model_summary, file.path(site_season_dir, "model_summary.csv"))
write_csv(coef_all,      file.path(site_season_dir, "species_model_coefficients.csv"))
write_csv(pred_all,      file.path(site_season_dir, "species_site_season_predictions.csv"))
write_csv(top_all,       file.path(site_season_dir, "species_top_site_seasons.csv"))

### 09. OPTIONAL: SEASON RANKINGS BY SPECIES ####

season_rankings <- pred_all %>%
  group_by(scientific_name, season) %>%
  summarise(
    mean_prob_across_sites = mean(prob, na.rm = TRUE),
    l95_mean = mean(l95, na.rm = TRUE),
    u95_mean = mean(u95, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(scientific_name) %>%
  arrange(scientific_name, desc(mean_prob_across_sites)) %>%
  mutate(season_rank = row_number()) %>%
  ungroup()

write_csv(
  season_rankings,
  file.path(site_season_dir, "species_season_rankings.csv")
)

### 10. OPTIONAL: SITE RANKINGS BY SPECIES ####

site_rankings <- pred_all %>%
  group_by(scientific_name, dive_site) %>%
  summarise(
    mean_prob_across_seasons = mean(prob, na.rm = TRUE),
    l95_mean = mean(l95, na.rm = TRUE),
    u95_mean = mean(u95, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(scientific_name) %>%
  arrange(scientific_name, desc(mean_prob_across_seasons)) %>%
  mutate(site_rank = row_number()) %>%
  ungroup()

write_csv(
  site_rankings,
  file.path(site_season_dir, "species_site_rankings.csv")
)

### 11. SIMPLE QA CHECKS ####

qa_trip_site <- trip_site %>%
  summarise(
    n_trip_site_rows = n(),
    n_trips = n_distinct(trip_id),
    n_sites = n_distinct(dive_site),
    mean_sites_per_trip = n() / n_distinct(trip_id)
  )

qa_detection <- trip_site_species %>%
  summarise(
    n_rows = n(),
    total_detected = sum(detected),
    prop_detected = mean(detected)
  )

write_csv(qa_trip_site, file.path(site_season_dir, "qa_trip_site.csv"))
write_csv(qa_detection, file.path(site_season_dir, "qa_detection.csv"))

### 12. PRINT QUICK SUMMARY ####

print(model_summary, n = Inf)

if (nrow(top_all) > 0) {
  top_all %>%
    group_by(scientific_name) %>%
    slice_head(n = 5) %>%
    print(n = Inf)
}


### 07_SITE_SEASON_PLOTS.R ####
### Purpose: heatmaps and quick summary plots for species-specific site x season encounter probabilities

library(tidyverse)

# expects from 00_RUN.R:
# - output_dir
stopifnot(exists("output_dir"))
stopifnot(dir.exists(output_dir))

site_season_dir <- file.path(output_dir, "site-season")
plot_dir <- file.path(site_season_dir, "plots")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

### 00. LOAD DATA ####

pred_all <- read_csv(
  file.path(site_season_dir, "species_site_season_predictions.csv"),
  show_col_types = FALSE
)

model_summary <- read_csv(
  file.path(site_season_dir, "model_summary.csv"),
  show_col_types = FALSE
)

site_rankings <- read_csv(
  file.path(site_season_dir, "species_site_rankings.csv"),
  show_col_types = FALSE
)

season_rankings <- read_csv(
  file.path(site_season_dir, "species_season_rankings.csv"),
  show_col_types = FALSE
)

### 01. BASIC FORMATTING ####

pred_all <- pred_all %>%
  mutate(
    scientific_name = as.character(scientific_name),
    dive_site = as.character(dive_site),
    season = factor(season, levels = c("early", "mid", "late", "other"))
  )

site_rankings <- site_rankings %>%
  mutate(
    scientific_name = as.character(scientific_name),
    dive_site = as.character(dive_site)
  )

season_rankings <- season_rankings %>%
  mutate(
    scientific_name = as.character(scientific_name),
    season = factor(season, levels = c("early", "mid", "late", "other"))
  )

season_labels <- c(
  early = "Early\n(Nov-Jan)",
  mid   = "Mid\n(Feb-Mar)",
  late  = "Late\n(Apr-May)",
  other = "Other"
)

### 02. HELPER: SAFE FILE NAME ####

safe_name <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_|_$", "")
}

### 03. SPECIES HEATMAPS ####

species_vec <- pred_all %>%
  distinct(scientific_name) %>%
  pull(scientific_name)

for (sp in species_vec) {
  
  dat_sp <- pred_all %>%
    filter(scientific_name == sp)
  
  site_order <- dat_sp %>%
    group_by(dive_site) %>%
    summarise(mean_prob = mean(prob, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(mean_prob)) %>%
    pull(dive_site)
  
  dat_sp <- dat_sp %>%
    mutate(
      dive_site = factor(dive_site, levels = rev(site_order))
    )
  
  p_heat <- ggplot(dat_sp, aes(x = season, y = dive_site, fill = prob)) +
    geom_tile(color = "white", linewidth = 0.2) +
    scale_x_discrete(labels = season_labels) +
    scale_fill_viridis_c(
      name = "Predicted\nprobability",
      limits = c(0, max(dat_sp$prob, na.rm = TRUE))
    ) +
    labs(
      title = sp,
      x = "Season",
      y = "Dive site"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid = element_blank(),
      axis.text.y = element_text(size = 8),
      plot.title = element_text(face = "italic")
    )
  
  ggsave(
    file.path(plot_dir, paste0("heatmap_", safe_name(sp), ".png")),
    p_heat,
    width = 7,
    height = max(4.5, 0.28 * n_distinct(dat_sp$dive_site)),
    dpi = 300
  )
}

### 04. TOP 10 SITE-SEASON COMBOS PER SPECIES ####

top10_site_season <- pred_all %>%
  group_by(scientific_name) %>%
  arrange(desc(prob), .by_group = TRUE) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 10) %>%
  ungroup() %>%
  select(scientific_name, rank, dive_site, season, prob, l95, u95)

write_csv(
  top10_site_season,
  file.path(site_season_dir, "species_top10_site_season_combos.csv")
)

### 05. FACETED SEASON PROFILES ####

top_sites_for_profiles <- site_rankings %>%
  group_by(scientific_name) %>%
  arrange(desc(mean_prob_across_seasons), .by_group = TRUE) %>%
  mutate(site_rank = row_number()) %>%
  filter(site_rank <= 5) %>%
  ungroup() %>%
  select(scientific_name, dive_site)

profile_dat <- pred_all %>%
  semi_join(top_sites_for_profiles, by = c("scientific_name", "dive_site")) %>%
  mutate(
    dive_site = fct_reorder(dive_site, prob, .fun = mean, .desc = TRUE)
  )

for (sp in species_vec) {
  
  dat_sp <- profile_dat %>%
    filter(scientific_name == sp)
  
  if (nrow(dat_sp) == 0) next
  
  p_prof <- ggplot(dat_sp, aes(x = season, y = prob, group = dive_site)) +
    geom_ribbon(aes(ymin = l95, ymax = u95, fill = dive_site), alpha = 0.15) +
    geom_line(aes(color = dive_site), linewidth = 0.8) +
    geom_point(aes(color = dive_site), size = 1.6) +
    scale_x_discrete(labels = season_labels) +
    labs(
      title = sp,
      x = "Season",
      y = "Predicted encounter probability",
      color = "Dive site",
      fill = "Dive site"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "italic"),
      panel.grid.minor = element_blank()
    )
  
  ggsave(
    file.path(plot_dir, paste0("season_profiles_", safe_name(sp), ".png")),
    p_prof,
    width = 7,
    height = 5,
    dpi = 300
  )
}

### 06. BEST SEASON PER SPECIES ####

best_season_tbl <- season_rankings %>%
  group_by(scientific_name) %>%
  arrange(desc(mean_prob_across_sites), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  select(
    scientific_name,
    best_season = season,
    mean_prob_across_sites,
    l95_mean,
    u95_mean
  )

write_csv(
  best_season_tbl,
  file.path(site_season_dir, "species_best_season.csv")
)

### 07. BEST SITE PER SPECIES ####

best_site_tbl <- site_rankings %>%
  group_by(scientific_name) %>%
  arrange(desc(mean_prob_across_seasons), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  select(
    scientific_name,
    best_site = dive_site,
    mean_prob_across_seasons,
    l95_mean,
    u95_mean
  )

write_csv(
  best_site_tbl,
  file.path(site_season_dir, "species_best_site.csv")
)

### 08. OVERVIEW PLOTS ####

p_best_season <- best_season_tbl %>%
  mutate(
    scientific_name = fct_reorder(scientific_name, mean_prob_across_sites)
  ) %>%
  ggplot(aes(x = scientific_name, y = mean_prob_across_sites)) +
  geom_errorbar(aes(ymin = l95_mean, ymax = u95_mean), width = 0) +
  geom_point() +
  coord_flip() +
  labs(
    x = NULL,
    y = "Mean predicted probability at best season",
    title = "Best season by species"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(face = "italic")
  )

ggsave(
  file.path(plot_dir, "overview_best_season_by_species.png"),
  p_best_season,
  width = 7,
  height = max(4.5, 0.32 * nrow(best_season_tbl)),
  dpi = 300
)

p_best_site <- best_site_tbl %>%
  mutate(
    scientific_name = fct_reorder(scientific_name, mean_prob_across_seasons)
  ) %>%
  ggplot(aes(x = scientific_name, y = mean_prob_across_seasons)) +
  geom_errorbar(aes(ymin = l95_mean, ymax = u95_mean), width = 0) +
  geom_point() +
  coord_flip() +
  labs(
    x = NULL,
    y = "Mean predicted probability at best site",
    title = "Best site by species"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(face = "italic")
  )

ggsave(
  file.path(plot_dir, "overview_best_site_by_species.png"),
  p_best_site,
  width = 7,
  height = max(4.5, 0.32 * nrow(best_site_tbl)),
  dpi = 300
)

### 09. OPERATOR SUMMARY TABLE ####

species_operator_summary <- best_site_tbl %>%
  left_join(
    best_season_tbl %>%
      select(scientific_name, best_season, mean_prob_across_sites),
    by = "scientific_name"
  ) %>%
  left_join(
    model_summary %>%
      select(scientific_name, n_detect, n_sites, n_years, converged, note),
    by = "scientific_name"
  ) %>%
  mutate(
    best_season = recode(
      as.character(best_season),
      early = "Nov-Jan",
      mid   = "Feb-Mar",
      late  = "Apr-May",
      other = "Other"
    )
  ) %>%
  transmute(
    scientific_name,
    converged,
    note,
    n_detect,
    n_sites,
    n_years,
    best_site,
    best_season,
    prob_best_site_avg = round(mean_prob_across_seasons, 3),
    prob_best_season_avg = round(mean_prob_across_sites, 3)
  ) %>%
  arrange(desc(prob_best_site_avg))

write_csv(
  species_operator_summary,
  file.path(site_season_dir, "species_operator_summary.csv")
)

print(species_operator_summary, n = Inf)
