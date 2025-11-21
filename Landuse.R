library(sf)
library(here)
library(mapview)
library(tidyverse)
# Read 2015 baseline GeoJSON
agri_2015 <- st_read(here("data/Ag_2015_Baseline.geojson"))
# Read 2020 update GeoJSON
agri_2020 <- st_read(here("data/Ag_2020_Update.geojson"))



#mapping2015data
mapview(agri_2015)
mapview(
  agri_2015,
  zcol = "cropcatego",   # field to color by
  legend = TRUE,
  layer.name = "2015 Agricultural Land Use"
)

#mapping2020data
library(dplyr)

agri_2015_clean <- agri_2015 %>%
  rename(crop = cropcatego)

agri_2020_clean <- agri_2020 %>%
  rename(crop = crops_2020)

acre_2015 <- agri_2015_clean %>%
  st_drop_geometry() %>%
  group_by(crop) %>%
  summarize(acres_2015 = sum(acreage, na.rm = TRUE))

acre_2020 <- agri_2020_clean %>%
  st_drop_geometry() %>%
  group_by(crop) %>%
  summarize(acres_2020 = sum(acreage, na.rm = TRUE))

acre_compare <- full_join(acre_2015, acre_2020, by = "crop") %>%
  replace_na(list(acres_2015 = 0, acres_2020 = 0))

acre_compare <- acre_compare %>%
  mutate(
    change_acres = acres_2020 - acres_2015,
    pct_change = ifelse(acres_2015 == 0, NA,
                        (change_acres / acres_2015) * 100)
  )

library(ggplot2)

ggplot(acre_compare, aes(x = reorder(crop,  pct_change),
                         y = pct_change,
                         fill = pct_change > 0)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Change in Crop Acreage (2020 vs 2015)",
    x = "Crop Category",
    y = "percent change"
  ) +
  scale_fill_manual(values = c("TRUE" = "forestgreen", "FALSE" = "firebrick"))

