# summarize person-day exposures and GRDI quartiles for each WHO region
rm(list=ls())
library(dplyr)
source("./02_code/20_setup/01_helper_functions.R")

# 0. Load data

# load tc_grdi, hurr_grdi from 1d_summary
grdi_dat = arrow::read_feather("./01_data/1a_raw/pop_wt_grdi_data/pop_wt_grdi_2020.feather") 

# Load data on GRDI
tc_grdi = readr::read_csv("./01_data/1d_summary/tc_grdi.csv")[, -1] # GRDI of ADM2 units exposed to TC
hurr_grdi = readr::read_csv("./01_data/1d_summary/hurr_grdi.csv")[, -1] # GRDI of ADM2 units exposed to Hurricanes

# load ADM2 related data from 1c_support
admin2_units <- load_adm2_lookup()

# 1. Summarize by WHO region
tc_summary = tc_grdi %>%
  group_by(region) %>% 
  summarise(total_pday_exp = sum(person_day_exposure))

hurr_summary = hurr_grdi %>% 
  group_by(region) %>% 
  summarise(total_pday_exp = sum(person_day_exposure))

## Europe
euro_tc = tc_grdi %>% filter(region == "Europe") %>%
  left_join(admin2_units, by = c("ADM2_id" = "shapeID")) # no hurricane in Europe during 2024

## Africa
afri_tc = tc_grdi %>% filter(region == "Africa") %>%
  left_join(admin2_units, by = c("ADM2_id" = "shapeID"))
afri_hurr = hurr_grdi %>% filter(region == "Africa") %>%
  left_join(admin2_units, by = c("ADM2_id" = "shapeID"))

## Western Pacific
wp_tc = tc_grdi %>% filter(region == "Western Pacific") %>% 
  left_join(admin2_units, by = c("ADM2_id" = "shapeID"))
wp_hurr = hurr_grdi %>% filter(region == "Western Pacific") %>% 
  left_join(admin2_units, by = c("ADM2_id" = "shapeID"))

## Americas
amer_tc = tc_grdi %>% filter(region == "Americas") %>% 
  left_join(admin2_units, by = c("ADM2_id" = "shapeID"))

amer_hurr = hurr_grdi %>% filter(region == "Americas") %>%
  left_join(admin2_units, by = c("ADM2_id" = "shapeID"))

## South-East Asia
se_asia_tc = tc_grdi %>% filter(region == "South-East Asia") %>%
  left_join(admin2_units, by = c("ADM2_id" = "shapeID"))
se_asia_hurr = hurr_grdi %>% filter(region == "South-East Asia") %>%
  left_join(admin2_units, by = c("ADM2_id" = "shapeID")) # no ADM2 exposed to hurricane

# 2. Summarize GRDI quantile of exposed region globally and by WHO Region
# 2a. Define quantiles
head(grdi_dat)
quantile(grdi_dat$pop_wt_grdi)
grdi_quantiles <- quantile(grdi_dat$pop_wt_grdi, probs = c(0.25, 0.5, 0.75))
q25 <- grdi_quantiles[1]
q50 <- grdi_quantiles[2]
q75 <- grdi_quantiles[3]

# 2b. Tropical cyclones
p1_tc = mean(tc_grdi$pop_wt_grdi <= q25)
p2_tc = mean(tc_grdi$pop_wt_grdi > q25 & tc_grdi$pop_wt_grdi <= q50)
p3_tc = mean(tc_grdi$pop_wt_grdi > q50 & tc_grdi$pop_wt_grdi <= q75)
p4_tc = mean(tc_grdi$pop_wt_grdi > q75)
p3_tc + p4_tc

p1_amer_tc <- mean(amer_tc$pop_wt_grdi <= q25)
p2_amer_tc <- mean(amer_tc$pop_wt_grdi > q25 & amer_tc$pop_wt_grdi <= q50)
p3_amer_tc <- mean(amer_tc$pop_wt_grdi > q50 & amer_tc$pop_wt_grdi <= q75)
p4_amer_tc <- mean(amer_tc$pop_wt_grdi > q75)
p3_amer_tc + p4_amer_tc

p1_wp_tc = mean(wp_tc$pop_wt_grdi <= q25)
p2_wp_tc = mean(wp_tc$pop_wt_grdi > q25 & wp_tc$pop_wt_grdi <= q50)
p3_wp_tc = mean(wp_tc$pop_wt_grdi > q50 & wp_tc$pop_wt_grdi <= q75)
p4_wp_tc = mean(wp_tc$pop_wt_grdi > q75)
p1_wp_tc + p2_wp_tc

p1_euro_tc = mean(euro_tc$pop_wt_grdi <= q25)
p2_euro_tc = mean(euro_tc$pop_wt_grdi > q25 & euro_tc$pop_wt_grdi <= q50)
p3_euro_tc = mean(euro_tc$pop_wt_grdi > q50 & euro_tc$pop_wt_grdi <= q75)
p4_euro_tc = mean(euro_tc$pop_wt_grdi > q75)
p1_euro_tc + p2_euro_tc

p1_se_asia_tc = mean(se_asia_tc$pop_wt_grdi <= q25)
p2_se_asia_tc = mean(se_asia_tc$pop_wt_grdi > q25 & se_asia_tc$pop_wt_grdi <= q50)
p3_se_asia_tc = mean(se_asia_tc$pop_wt_grdi > q50 & se_asia_tc$pop_wt_grdi <= q75)
p4_se_asia_tc = mean(se_asia_tc$pop_wt_grdi > q75)
p3_se_asia_tc + p4_se_asia_tc

p1_afri_tc = mean(afri_tc$pop_wt_grdi <= q25)
p2_afri_tc = mean(afri_tc$pop_wt_grdi > q25 & afri_tc$pop_wt_grdi <= q50)
p3_afri_tc = mean(afri_tc$pop_wt_grdi > q50 & afri_tc$pop_wt_grdi <= q75)
p4_afri_tc = mean(afri_tc$pop_wt_grdi > q75)
p3_afri_tc + p4_afri_tc

# 2c. Hurricanes
p1_hurr = mean(hurr_grdi$pop_wt_grdi <= q25)
p2_hurr = mean(hurr_grdi$pop_wt_grdi > q25 & hurr_grdi$pop_wt_grdi <= q50)
p3_hurr <- mean(hurr_grdi$pop_wt_grdi > q50 & hurr_grdi$pop_wt_grdi <= q75)
p4_hurr <- mean(hurr_grdi$pop_wt_grdi > q75)
p1_hurr + p2_hurr

p1_amer_hurr = mean(amer_hurr$pop_wt_grdi <= q25)
p2_amer_hurr = mean(amer_hurr$pop_wt_grdi > q25 & amer_hurr$pop_wt_grdi <= q50)
p3_amer_hurr <- mean(amer_hurr$pop_wt_grdi > q50 & amer_hurr$pop_wt_grdi <= q75)
p4_amer_hurr <- mean(amer_hurr$pop_wt_grdi > q75)
p3_amer_hurr + p4_amer_hurr

p1_wp_hurr = mean(wp_hurr$pop_wt_grdi <= q25)
p2_wp_hurr = mean(wp_hurr$pop_wt_grdi > q25 & wp_hurr$pop_wt_grdi <= q50)
p3_wp_hurr = mean(wp_hurr$pop_wt_grdi > q50 & wp_hurr$pop_wt_grdi <= q75)
p4_wp_hurr = mean(wp_hurr$pop_wt_grdi > q75)
p1_wp_hurr + p2_wp_hurr

p1_afri_hurr = mean(afri_hurr$pop_wt_grdi <= q25)
p2_afri_hurr = mean(afri_hurr$pop_wt_grdi > q25 & afri_hurr$pop_wt_grdi <= q50)
p3_afri_hurr = mean(afri_hurr$pop_wt_grdi > q50 & afri_hurr$pop_wt_grdi <= q75)
p4_afri_hurr = mean(afri_hurr$pop_wt_grdi > q75)
p3_afri_hurr + p4_afri_hurr
