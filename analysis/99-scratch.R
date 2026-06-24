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