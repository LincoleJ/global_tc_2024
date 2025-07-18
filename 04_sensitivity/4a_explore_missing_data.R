# Investigate missing data issues for person-day exposure
rm(list=ls())

# 0a. Load packages
library(dplyr)

# 0b. Load data
all_pday_tc_exp <- list()
for (year in 1980:2024) {
  # load person-day exposures
  pday_tc_exp = readr::read_csv(paste0("./01_data/processed_pday_exp_data/pday_tc_exp_",
                                       year, ".csv")) %>% 
    mutate(year = year)
  all_pday_tc_exp[[as.character(year)]] <- pday_tc_exp
}
all_pday_nas <- bind_rows(all_pday_tc_exp)[, -1] %>%
  filter(if_any(everything(), is.na)) 

admin2_units = sf::read_sf("./01_data/adm_boundaries/geoBoundariesCGAZ_ADM2.geojson")

adm2_ids = data.frame(shapeName = admin2_units$shapeName,
                      shapeID = admin2_units$shapeID,
                      shapeGroup = admin2_units$shapeGroup,
                      shapeType = admin2_units$shapeType)

# get the list of missing entries
na_adm2s = adm2_ids %>%
  filter(shapeID %in% unique(all_pday_nas$ADM2_id))

all_na_dat = left_join(all_pday_nas, adm2_ids, by = c("ADM2_id" = "shapeID"))

# fine