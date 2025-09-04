# get the coordinates limits of exposed regions in according to WHO region
rm(list=ls())

# 0a. Load packages
library(dplyr)
library(tidyr)
library(sf)

# 0b. Load datasets
# Administrative unit boundaries
source("./02_code/00_setup/01_helper_functions.R")
adm2_boundaries = load_adm2_boundaries()

# person-day exposure
pday_tc_2024 = readr::read_csv("./01_data/1d_summary/processed_pday_exp_data/pday_tc_exp_2024.csv")[, -1]
pday_hurr_2024 = readr::read_csv("./01_data/1d_summary/processed_pday_exp_data/pday_hurr_exp_2024.csv")[ -1]

# WHO region
adm2_key = load_adm2_who_mapping()

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
