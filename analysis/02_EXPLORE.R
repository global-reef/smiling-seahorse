### basic exploration: time + space + taxa ####

library(tidyverse)
library(lubridate)

## helper: year-month bin
elasmos <- elasmos %>%
  mutate(ym = floor_date(sighting_date, "month"))

## 1) overall time series: Myanmar vs Thailand (monthly counts)
elasmos %>%
  filter(country %in% c("Myanmar", "Thailand")) %>%
  count(country, ym, name = "n") %>%
  ggplot(aes(ym, n)) +
  geom_line() +
  geom_smooth(se = TRUE, method = "loess", span = 0.5) +
  facet_wrap(~country, ncol = 1, scales = "free_y") + coord_cartesian(ylim = c(0, 25)) +
  labs(x = NULL, y = "Sightings (monthly)", title = "Elasmo sightings over time")

## 2) Myanmar breakdown: Burma Banks vs Mergui vs other (monthly)
elasmos %>%
  filter(country == "Myanmar") %>%
  mutate(
    my_subregion = case_when(
      str_detect(str_to_lower(dive_site), "burma banks|burma\\s*banks") ~ "Burma Banks",
      str_detect(str_to_lower(region), "mergui") ~ "Mergui",
      TRUE ~ "Other Myanmar"
    )
  ) %>%
  count(my_subregion, ym, name = "n") %>%
  ggplot(aes(ym, n)) +
  geom_line() +
  geom_smooth(se = TRUE, method = "loess", span = 0.6) +
  facet_wrap(~my_subregion, ncol = 1, scales = "free_y") +
  labs(x = NULL, y = "Sightings (monthly)", title = "Myanmar subregions over time")

## 3) sharks vs rays distribution by country (annual to reduce noise)
elasmos %>%
  filter(country %in% c("Myanmar", "Thailand")) %>%
  mutate(year = year(sighting_date),
         group2 = case_when(
           str_detect(str_to_lower(group), "ray") ~ "ray",
           str_detect(str_to_lower(group), "shark") ~ "shark",
           TRUE ~ "other"
         )) %>%
  count(country, year, group2, name = "n") %>%
  ggplot(aes(year, n)) +
  geom_line() +
  geom_smooth(se = TRUE, method = "loess", span = 0.8) +
  facet_grid(group2 ~ country, scales = "free_y") + coord_cartesian(ylim = c(0, 30)) +
  labs(x = NULL, y = "Sightings (annual)", title = "Sharks vs rays through time")

## 4) top species in Myanmar, and their time series (annual)
top_spp_myanmar <- elasmos %>%
  filter(country == "Myanmar") %>%
  count(species, sort = TRUE) %>%
  slice_head(n = 12) %>%
  pull(species)

elasmos %>%
  filter(country == "Myanmar", species %in% top_spp_myanmar) %>%
  mutate(year = year(sighting_date)) %>%
  count(species, year, name = "n") %>%
  ggplot(aes(year, n)) +
  geom_line() +
  geom_smooth(se = FALSE, method = "loess", span = 1) +
  facet_wrap(~species, scales = "free_y") +
  labs(x = NULL, y = "Sightings (annual)", title = "Top Myanmar species trends")

### 5) top species time series: Myanmar vs Thailand ####

top_spp <- elasmos %>%
  filter(country == "Myanmar") %>%
  count(species, sort = TRUE) %>%
  slice_head(n = 12) %>%
  pull(species)

elasmos %>%
  filter(country %in% c("Myanmar", "Thailand"),
         species %in% top_spp) %>%
  mutate(year = lubridate::year(sighting_date)) %>%
  count(country, species, year, name = "n") %>%
  ggplot(aes(year, n, colour = country)) +
  geom_point(alpha = 0.6, size = 1.8) +
  geom_smooth(se = FALSE, method = "loess", span = 1) +
  facet_wrap(~species, scales = "free_y") +
  scale_colour_manual(
    values = c("Myanmar" = "lightblue", "Thailand" = "orange")
  ) +
  labs(
    x = NULL,
    y = "Sightings (annual)",
    colour = NULL,
    title = "Top Myanmar species: annual sightings (with Thailand comparison)"
  )


#### check effort and distributions ##### 
# effort 
elasmos %>%
  count(country, year) %>%
  ggplot(aes(year, n, colour = country)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  labs(x = NULL, y = "Total sightings")

# spatial distr
elasmos %>%
  mutate(
    subregion = case_when(
      str_detect(str_to_lower(dive_site), "burma") ~ "Burma Banks",
      country == "Myanmar" ~ "Mergui",
      TRUE ~ "Thailand"
    )
  ) %>%
  count(subregion, species) %>%
  group_by(subregion) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(prop, species, fill = subregion)) +
  geom_col(position = "dodge") +
  labs(x = "Proportion of sightings", y = NULL)

# shark vs rays by country 
elasmos %>%
  count(country, group) %>%
  group_by(country) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(country, prop, fill = group)) +
  geom_col() +
  labs(y = "Proportion of sightings")

# species turnover (myanmar only) 
elasmos %>%
  filter(country == "Myanmar") %>%
  mutate(year = year(sighting_date)) %>%
  count(year, species) %>%
  count(year, name = "species_richness") %>%
  ggplot(aes(year, species_richness)) +
  geom_line() +
  geom_point() +
  labs(y = "Species richness")

# species ranges
elasmos %>%
  count(species, dive_site) %>%
  count(species, name = "n_sites") %>%
  arrange(desc(n_sites)) %>%
  slice_head(n = 15)

# seasonality 
elasmos %>%
  mutate(month = month(sighting_date, label = TRUE)) %>%
  count(country, month) %>%
  ggplot(aes(month, n, fill = country)) +
  geom_col(position = "dodge") +
  labs(y = "Sightings")

