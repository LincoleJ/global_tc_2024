# Run this ONCE after downloading data or when data updates
library(sf)
library(dplyr)

# 1. Process ADM2 boundaries
admin2_units <- sf::read_sf("./01_data/1c_support/adm_boundaries/geoBoundariesCGAZ_ADM2.geojson")

# Save lightweight lookup table (no geometries)
adm2_lookup <- data.frame(
  shapeID = admin2_units$shapeID,
  ctry_code = admin2_units$shapeGroup,
  shapeName = admin2_units$shapeName
) %>% distinct()
saveRDS(adm2_lookup, "./01_data/1d_summary/adm2_lookup.rds")

# Save full geometries separately
saveRDS(admin2_units, "./01_data/1d_summary/adm2_boundaries.rds")

# 2. Process WHO regions with ADM2 mapping
who_key <- readr::read_csv("./01_data/1c_support/who-regions/who-regions.csv") %>%
  mutate(`World regions according to WHO` = 
           stringr::str_remove(`World regions according to WHO`, " \\(WHO\\)"))
colnames(who_key) <- c("country", "ctry_code", "year", "who_region")

adm2_who_key <- adm2_lookup %>%
  left_join(who_key, by = "ctry_code") %>%
  select(-year) %>%
  mutate(who_region = case_when(
    ctry_code == "TWN" ~ "Western Pacific",
    ctry_code == "VCT" ~ "Americas",
    ctry_code == "GRL" | ctry_code == "XKX" | ctry_code == "LIE" ~ "Europe",
    country == "Gaza Strip" | country == "West Bank" ~ "Eastern Mediterranean",
    TRUE ~ who_region
  )) %>%
  mutate(who_region = case_when(ctry_code == "TWN" ~ "Western Pacific",
                                ctry_code == "VCT" ~ "Americas",
                                ctry_code == "GRL" | ctry_code == "XKX" | 
                                  ctry_code == "LIE" | ctry_code == "VAT" ~ "Europe",
                                shapeName == "Gaza Strip" | shapeName == "West Bank" ~ "Eastern Mediterranean",
                                TRUE ~ who_region)) %>%
  mutate(country = case_when(ctry_code == "TWN" ~ "Taiwan",
                             ctry_code == "VCT" ~ "Saint Vincent and the Grenadines",
                             ctry_code == "GRL" ~ "Greenland",
                             ctry_code == "XKX" ~ "Republic of Kosovo",
                             ctry_code == "LIE" ~ "Liechtenstein",
                             ctry_code == "VAT" ~ "Vatican City State",
                             TRUE ~ country)) 
saveRDS(adm2_who_key, "./01_data/1d_summary/adm2_who_key.rds")

# 3. Process ADM0 boundaries
adm0_boundaries <- sf::read_sf("./01_data/1c_support/adm_boundaries/geoBoundariesCGAZ_ADM0.geojson") %>%
  filter(shapeGroup != "ATA")
saveRDS(adm0_boundaries, "./01_data/1d_summary/adm0_boundaries.rds")
