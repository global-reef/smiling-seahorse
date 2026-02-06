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
  geom_smooth(se = TRUE, method = "loess", span = 0.6) +
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
  facet_grid(group2 ~ country, scales = "free_y") +
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
  geom_smooth(se = FALSE, method = "loess", span = 0.8) +
  facet_wrap(~species, scales = "free_y") +
  labs(x = NULL, y = "Sightings (annual)", title = "Top Myanmar species trends")
