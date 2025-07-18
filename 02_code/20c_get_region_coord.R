# get the coordinates limits of exposed regions in according to WHO region
rm(list=ls())

# 0a. Load packages
library(dplyr)

# 0b. Load datasets
# Administrative unit boundaries
adm2_boundaries = sf::read_sf("./01_data/1c_support/adm_boundaries/geoBoundariesCGAZ_ADM2.geojson")

# person-day exposure
pday_tc_2024 = readr::read_csv("./01_data/1d_summary/processed_pday_exp_data/pday_tc_exp_2024.csv")[, -1]
pday_hurr_2024 = readr::read_csv("./01_data/1d_summary/processed_pday_exp_data/pday_hurr_exp_2024.csv")[ -1]

# WHO region
library(countrycode)
who_key = readr::read_csv("./01_data/1c_support/who-regions/who-regions.csv") %>%
  mutate(`World regions according to WHO` = 
           stringr::str_remove(`World regions according to WHO`, " \\(WHO\\)"))
colnames(who_key) = c("country", "ctry_code", "year", "who_region")
adm2_key = data.frame(shapeID = adm2_boundaries$shapeID,
                      ctry_code = adm2_boundaries$shapeGroup) %>%
  distinct()
adm2_key = left_join(adm2_key, who_key, by = c("ctry_code"))
adm2_key = adm2_key %>% select(shapeID, who_region)

# 1. Merge exposure data with ADM2 boundary and filter non-zero exposures
adm2_nz_tc_exp = merge(adm2_boundaries, pday_tc_2024, by.x = "shapeID", by.y = "ADM2_id") %>% 
  filter(total_person_day_exposure != 0) 
adm2_nz_hurr_exp = merge(adm2_boundaries, pday_hurr_2024, by.x = "shapeID", by.y = "ADM2_id") %>% 
  filter(total_person_day_exposure != 0)

# 2. Merge exposure data with who region designation
exp_adm2_by_region = left_join(adm2_nz_tc_exp, adm2_key, by = c("shapeID")) %>% 
  mutate(who_region = ifelse(shapeGroup == "TWN", "Western Pacific", who_region)) %>%
  mutate(who_region = ifelse(shapeGroup == "VCT", "Americas", who_region)) 

bbox_by_region <- exp_adm2_by_region %>%
  group_by(who_region) %>%
  summarise(geometry = sf::st_union(geometry)) %>%  # combine geometries per region
  mutate(bbox = purrr::map(geometry, st_bbox)) %>% 
  unnest_wider(bbox) %>%
  select(who_region, xmin, ymin, xmax, ymax)

# 3. Save for further use
write.csv(bbox_by_region, "./01_data/1d_summary/bbox_by_region.csv")
