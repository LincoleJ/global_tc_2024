# generate person-day exposures (TC and hurricane) from 1980-2024
rm(list=ls())

# 0a. Load packages
library(dplyr)
library(countrycode)

# 1. Generate person-day exposures for a) TC and b) Hurricane exposures
for (year in 1980:2024) {
  ## hurricane tracks
  storm_track_dat = readRDS(paste0("./01_data/1a_raw/global_hurr_dat/global_storm_winds_", year, ".csv"))
  tc_exposures <- storm_track_dat %>% filter(vmax_sust >= 17.4911)
  hurr_exposures = storm_track_dat %>% filter(vmax_sust >= 32.9244)
  
  # mutate day
  tc_exposures = tc_exposures %>% mutate(day = as.Date(date_time_max_wind))
  hurr_exposures = hurr_exposures %>% mutate(day = as.Date(date_time_max_wind))
  
  ## population
  if (year < 2020) {
    adm2_pop = readr::read_csv(paste0("./01_data/1b_intermediate/pop_by_adm2_interpolated/adm2_pop_", year, ".csv"))[, -1] 
    colnames(adm2_pop) = c("shapeID", "pop")
    
    # join population data with storm wind data
    tc_exposures = left_join(tc_exposures, adm2_pop, by = c("ADM2_id" = "shapeID"))
    hurr_exposures = left_join(hurr_exposures, adm2_pop, by = c("ADM2_id" = "shapeID"))
    
    # 1. Calculate person-day exposures for each ADM2
    person_day_tc_exposure <- tc_exposures %>%
      group_by(ADM2_id) %>%
      summarise(total_exposure_days = n_distinct(day), # count unique exposure days for each ADM2
                total_population = max(pop)) %>% 
      mutate(total_person_day_exposure = total_exposure_days * total_population) %>%
      mutate(log_total_person_day_exposure = 
               case_when(total_person_day_exposure == 0 ~ 0,
                         total_person_day_exposure != 0 ~ log(total_person_day_exposure)))
    
    person_day_hurr_exposure <- hurr_exposures %>%
      group_by(ADM2_id) %>%
      summarise(total_exposure_days = n_distinct(day),
                total_population = max(pop)) %>%
      mutate(total_person_day_exposure = total_exposure_days * total_population) %>%
      mutate(log_total_person_day_exposure = 
               case_when(total_person_day_exposure == 0 ~ 0,
                         total_person_day_exposure != 0 ~ log(total_person_day_exposure)))
    
    # 2. Save datasets
    write.csv(person_day_tc_exposure, 
              paste0("./01_data/1d_summary/processed_pday_exp_data/pday_tc_exp_", year, ".csv"))
    write.csv(person_day_hurr_exposure,
              paste0("./01_data/1d_summary/processed_pday_exp_data/pday_hurr_exp_", year, ".csv"))
  } else {
    adm2_pop = readr::read_csv(paste0("./01_data/1b_intermediate/pop_by_adm2_interpolated/adm2_pop_", 2020, ".csv"))[, -1] 
    colnames(adm2_pop) = c("shapeID", "pop")
    
    # join population data with storm wind data
    tc_exposures = left_join(tc_exposures, adm2_pop, by = c("ADM2_id" = "shapeID"))
    hurr_exposures = left_join(hurr_exposures, adm2_pop, by = c("ADM2_id" = "shapeID"))
    
    # 1. Calculate person-day exposures for each ADM2
    person_day_tc_exposure <- tc_exposures %>%
      group_by(ADM2_id) %>%
      summarise(total_exposure_days = n_distinct(day), # count unique exposure days for each ADM2
                total_population = max(pop)) %>% 
      mutate(total_person_day_exposure = total_exposure_days * total_population) %>%
      mutate(log_total_person_day_exposure = 
               case_when(total_person_day_exposure == 0 ~ 0,
                         total_person_day_exposure != 0 ~ log(total_person_day_exposure)))
    
    person_day_hurr_exposure <- hurr_exposures %>%
      group_by(ADM2_id) %>%
      summarise(total_exposure_days = n_distinct(day),
                total_population = max(pop)) %>%
      mutate(total_person_day_exposure = total_exposure_days * total_population) %>%
      mutate(log_total_person_day_exposure = 
               case_when(total_person_day_exposure == 0 ~ 0,
                         total_person_day_exposure != 0 ~ log(total_person_day_exposure)))
    
    # 2. Save datasets
    write.csv(person_day_tc_exposure, 
              paste0("./01_data/1d_summary/processed_pday_exp_data/pday_tc_exp_", year, ".csv"))
    write.csv(person_day_hurr_exposure,
              paste0("./01_data/1d_summary/processed_pday_exp_data/pday_hurr_exp_", year, ".csv"))
  }
}