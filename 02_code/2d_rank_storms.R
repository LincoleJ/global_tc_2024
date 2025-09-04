# rank the most-exposed storms in calendar year 2024
rm(list=ls())

# 0. Load data
source("./02_code/20_setup/01_helper_functions.R")
admin2_units <- load_adm2_lookup()  
adm2_key = admin2_units %>%
  distinct()
year = 2024
storm_track_dat = readRDS(paste0("./01_data/1a_raw/global_hurr_dat/global_storm_winds_", year, ".csv"))
tc_exposures <- storm_track_dat %>% filter(vmax_sust >= 17.4911)
hurr_exposures = storm_track_dat %>% filter(vmax_sust >= 32.9244)

# mutate day
tc_exposures = tc_exposures %>% mutate(day = as.Date(date_time_max_wind))
hurr_exposures = hurr_exposures %>% mutate(day = as.Date(date_time_max_wind))

adm2_pop = readr::read_csv(paste0("./01_data/1b_intermediate/pop_by_adm2_interpolated/adm2_pop_", 2020, ".csv"))[, -1] 
colnames(adm2_pop) = c("shapeID", "pop")

# join population data with storm wind data
tc_exposures = left_join(tc_exposures, adm2_pop, by = c("ADM2_id" = "shapeID"))
hurr_exposures = left_join(hurr_exposures, adm2_pop, by = c("ADM2_id" = "shapeID"))

# 1. Calculate person-day exposure and display the storms causing the most person-day exposures
person_day_tc_exposure <- tc_exposures %>%
  mutate(pop = case_when(is.na(pop) ~ 0, 
                         TRUE ~ pop)) %>%
  group_by(ADM2_id, storm_id) %>%
  summarise(total_exposure_days = n_distinct(day), # count unique exposure days for each ADM2
            total_population = max(pop), 
            .groups = "drop") %>% 
  mutate(total_person_day_exposure = total_exposure_days * total_population) 

# filter NAs
pday_tc_exp_nas = person_day_tc_exposure[!complete.cases(person_day_tc_exposure), ]

tc_pday_by_storm = person_day_tc_exposure %>% group_by(storm_id) %>%
  summarise(total_pday = sum(total_person_day_exposure))
top10_storms <- tc_pday_by_storm %>%
  filter(!is.na(total_pday)) %>%
  arrange(desc(total_pday)) %>%
  slice_head(n = 10)

top10_tc_storms = person_day_tc_exposure %>% 
  filter(storm_id %in% top10_storms$storm_id) %>%
  left_join(adm2_key, by = c("ADM2_id" = "shapeID")) %>%
  select(storm_id, ctry_code) %>%
  distinct()

person_day_hurr_exposure <- hurr_exposures %>%
  mutate(pop = case_when(is.na(pop) ~ 0, 
                         TRUE ~ pop)) %>%
  group_by(ADM2_id, storm_id) %>%
  summarise(total_exposure_days = n_distinct(day),
            total_population = max(pop),
            .groups = "drop") %>%
  mutate(total_person_day_exposure = total_exposure_days * total_population) 
hurr_pday_by_storm = person_day_hurr_exposure %>% group_by(storm_id) %>%
  summarise(total_pday = sum(total_person_day_exposure))
top10_storms <- hurr_pday_by_storm %>%
  filter(!is.na(total_pday)) %>%
  arrange(desc(total_pday)) %>%
  slice_head(n = 11)

top10_hurr_storms = person_day_hurr_exposure %>% 
  filter(storm_id %in% top10_storms$storm_id) %>%
  left_join(adm2_key, by = c("ADM2_id" = "shapeID")) %>%
  select(storm_id, ctry_code) %>%
  distinct()

# 2. Save to shiny_app/ folder for implementation
# Define the output directory
output_dir <- "./shiny_app/processed_data_v4/"

# Save the summarized storm data
saveRDS(tc_pday_by_storm, file.path(output_dir, "tc_pday_by_storm.rds"))
saveRDS(hurr_pday_by_storm, file.path(output_dir, "hurr_pday_by_storm.rds"))
