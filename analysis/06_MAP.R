### Packages

library(sf)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(patchwork)

### Colours

country_cols <- c("Myanmar" = "#007A87", "Thailand" = "#66BFA6")
### Text sizes

axis_text_size  <- 8
axis_title_size <- 10
panel_tag_size  <- 12
legend_text_size <- 8


### Base map

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

countries <- world |>
  filter(admin %in% c("Myanmar", "Thailand", "Malaysia", "India", "Bangladesh",
                      "Laos", "Cambodia", "Vietnam"))

### Study-region boxes

bbox_df <- data.frame(
  region  = c("Mergui Archipelago", "Burma Banks", "North Andaman", "South Andaman"),
  country = c("Myanmar", "Myanmar", "Thailand", "Thailand"),
  xmin    = c(97.5, 96.6, 97.7, 98.2),
  xmax    = c(98.8, 97.3, 98.6, 99.5),
  ymin    = c(10.1, 10.0, 8.8, 6.2),
  ymax    = c(12.8, 11.0, 10.0, 8.6)
)

make_bbox <- function(xmin, xmax, ymin, ymax) {
  st_polygon(list(matrix(c(xmin, ymin, xmax, ymin, xmax, ymax, xmin, ymax, xmin, ymin),
                         ncol = 2, byrow = TRUE)))
}

boxes <- st_sf(
  bbox_df,
  geometry = st_sfc(
    mapply(make_bbox, bbox_df$xmin, bbox_df$xmax, bbox_df$ymin, bbox_df$ymax,
           SIMPLIFY = FALSE),
    crs = 4326
  )
)

### Extents

main_extent <- c(xmin = 95, xmax = 100.4, ymin = 5.8, ymax = 14.2)
locator_extent <- c(xmin = 90, xmax = 105, ymin = 0, ymax = 24)
locator_box <- st_as_sfc(st_bbox(main_extent, crs = 4326))

### Panel B: main study-region map

p_main <- ggplot() +
  geom_sf(data = countries, fill = "grey92", colour = "grey60", linewidth = 0.25) +
  geom_sf(data = boxes, aes(fill = country), colour = NA, alpha = 0.35) +
  scale_fill_manual(values = country_cols, name = NULL) +
  coord_sf(xlim = main_extent[c("xmin", "xmax")],
           ylim = main_extent[c("ymin", "ymax")],
           expand = FALSE) +
  scale_x_continuous(breaks = c(96, 98, 100),
                     labels = \(x) paste0(x, "°E")) +
  scale_y_continuous(breaks = c(6, 8, 10, 12, 14),
                     labels = \(y) paste0(y, "°N")) +
  labs(x = "Longitude", y = "Latitude") +
  theme_classic(base_size = axis_title_size) +
  theme(
    axis.line = element_blank(),
    axis.text = element_text(size = axis_text_size, colour = "black"),
    axis.title = element_text(size = axis_title_size, colour = "black"),
    panel.border = element_rect(fill = NA, colour = "grey35", linewidth = 0.45),
    legend.position = "bottom",
    legend.text = element_text(size = legend_text_size),
    legend.key.width = unit(0.8, "cm"),
    legend.key.height = unit(0.35, "cm"),
    plot.margin = margin(3, 3, 3, 3)
  )

### Locator countries

world_locator <- world |>
  mutate(country = case_when(
    admin == "Myanmar" ~ "Myanmar",
    admin == "Thailand" ~ "Thailand",
    TRUE ~ NA_character_
  ))

world_other <- world_locator |> filter(is.na(country))
world_focus <- world_locator |> filter(!is.na(country))
### Panel A: locator map

p_locator <- ggplot() +
  geom_sf(data = world, fill = "grey94", colour = "grey72", linewidth = 0.18) +
  geom_sf(data = locator_box, fill = NA, colour = "black", linewidth = 0.5) +
  scale_fill_manual(values = country_cols, guide = "none") +
  coord_sf(xlim = locator_extent[c("xmin", "xmax")],
           ylim = locator_extent[c("ymin", "ymax")],
           expand = FALSE) +
  scale_x_continuous(breaks = c(90, 95, 100, 105),
                     labels = \(x) paste0(x, "°E")) +
  scale_y_continuous(breaks = c(0, 10, 20),
                     labels = \(y) paste0(y, "°N")) +
  labs(x = "Longitude", y = "Latitude") +
  theme_classic(base_size = axis_title_size) +
  theme(
    axis.line = element_blank(),
    axis.text = element_text(size = axis_text_size, colour = "black"),
    axis.title = element_text(size = axis_title_size, colour = "black"),
    panel.border = element_rect(fill = NA, colour = "grey35", linewidth = 0.45),
    plot.margin = margin(3, 3, 3, 3)
  )

### Combine and export

fig_map <- p_locator + p_main +
  plot_layout(widths = c(1, 1), guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(
    plot.tag = element_text(size = panel_tag_size, face = "bold"),
    plot.tag.position = c(0.02, 0.98),
    legend.position = "bottom"
  )

fig_map

ggsave("fig_study_area_bounding_boxes.png", fig_map,
       width = 180, height = 105, units = "mm", dpi = 600)

ggsave("fig_study_area_bounding_boxes.pdf", fig_map,
       width = 180, height = 105, units = "mm")