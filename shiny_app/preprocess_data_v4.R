rm(list=ls())
library(dplyr)
library(sf)
library(readr)
library(rmapshaper)

cat("Starting OPTIMIZED data preprocessing...\n")

# ========================================
# 1. LOAD RAW DATA
# ========================================

cat("Loading raw data files...\n")
all_storms <- readr::read_csv("./01_data/1a_raw/IBTrACS_raw_data/ibtracs.last3years.list.v04r01.csv", show_col_types = FALSE)
adm2_boundaries_raw <- sf::read_sf("./01_data/1c_support/adm_boundaries/geoBoundariesCGAZ_ADM2.geojson")
adm0_boundaries_raw <- sf::read_sf("./01_data/1c_support/adm_boundaries/geoBoundariesCGAZ_ADM0.geojson") %>% filter(shapeGroup != "ATA")
pday_tc_2024 <- readr::read_csv("./01_data/1d_summary/processed_pday_exp_data/pday_tc_exp_2024.csv", show_col_types = FALSE)[, -1]
pday_hurr_2024 <- readr::read_csv("./01_data/1d_summary/processed_pday_exp_data/pday_hurr_exp_2024.csv", show_col_types = FALSE)[, -1]
grdi_dat_raw <- arrow::read_feather("./01_data/1a_raw/pop_wt_grdi_data/pop_wt_grdi_2020.feather")
global_storm_winds_2024 <- readRDS("./01_data/1a_raw/global_hurr_dat/global_storm_winds_2024.csv")


# ========================================
# 2. ### OPTIMIZATION ### SIMPLIFY GEOMETRIES IMMEDIATELY
# ========================================

cat("Simplifying geometries for faster processing...\n")
# Simplify early and aggressively. This makes all subsequent steps much faster.
# For ADM2 boundaries this was done in local terminal; simplified to 0.05% geometry
adm2_boundaries <- sf::read_sf("./01_data/1c_support/adm_boundaries/adm2_boundaries_simplified.geojson")
adm0_boundaries_wgs84 <- rmapshaper::ms_simplify(adm0_boundaries_raw, keep = 0.1, keep_shapes = TRUE) %>%
  st_transform(4326) # Transform once here

# ========================================
# 3. PROCESS TRACK AND GRDI DATA
# ========================================

cat("Processing track and GRDI data...\n")
# --- GRDI Processing ---
grdi_quantiles <- quantile(grdi_dat_raw$pop_wt_grdi, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
grdi_dat <- grdi_dat_raw %>%
  mutate(quartile = case_when(
    pop_wt_grdi <= grdi_quantiles[1] ~ "low",
    pop_wt_grdi > grdi_quantiles[1] & pop_wt_grdi <= grdi_quantiles[2] ~ "moderately low",
    pop_wt_grdi > grdi_quantiles[2] & pop_wt_grdi <= grdi_quantiles[3] ~ "moderately high",
    pop_wt_grdi > grdi_quantiles[3] ~ "high"
  ))

# --- Track Processing ---
hurr_tracks_2024 <- all_storms %>%
  transmute(
    unique_identifier = SID,
    storm_id = NAME,
    date = ISO_TIME,
    latitude = USA_LAT,
    longitude = USA_LON,
    wind = USA_WIND
  ) %>%
  na.omit() %>%
  filter(substr(date, 1, 4) == "2024") %>%
  mutate(
    storm_id = paste0(tools::toTitleCase(tolower(storm_id)), "-", substr(date, 1, 4)),
    month = as.numeric(substr(date, 6, 7))
  )

tc_track_names <- global_storm_winds_2024 %>% filter(vmax_sust >= 17.4911) %>% pull(sid) %>% unique()
ht_track_names <- global_storm_winds_2024 %>% filter(vmax_sust >= 32.9244) %>% pull(sid) %>% unique()

tc_tracks_2024 <- hurr_tracks_2024 %>% filter(unique_identifier %in% tc_track_names)
ht_tracks_2024 <- hurr_tracks_2024 %>% filter(unique_identifier %in% ht_track_names)

# --- Storm to ADM2 Mapping ---
adm2_tc_storms <- global_storm_winds_2024 %>%
  filter(vmax_sust >= 17.4911) %>%
  group_by(ADM2_id) %>%
  summarise(contributing_storms = list(unique(storm_id)), .groups = "drop")

adm2_ht_storms <- global_storm_winds_2024 %>%
  filter(vmax_sust >= 32.9244) %>%
  group_by(ADM2_id) %>%
  summarise(contributing_storms = list(unique(storm_id)), .groups = "drop")

# ========================================
# 4. MERGE AND TRANSFORM DATA
# ========================================

cat("Merging and transforming final datasets...\n")
# --- TC Data ---
adm2_tc_exp_wgs84 <- adm2_boundaries %>%
  inner_join(pday_tc_2024, by = c("shapeID" = "ADM2_id")) %>%
  filter(total_person_day_exposure != 0) %>%
  left_join(grdi_dat, by = "shapeID") %>%
  left_join(adm2_tc_storms, by = c("shapeID" = "ADM2_id")) %>%
  mutate(log_exposure = log10(total_person_day_exposure + 0.1)) %>%
  st_transform(4326)

# --- Hurricane Data ---
adm2_hurr_exp_wgs84 <- adm2_boundaries %>%
  inner_join(pday_hurr_2024, by = c("shapeID" = "ADM2_id")) %>%
  filter(total_person_day_exposure != 0) %>%
  left_join(grdi_dat, by = "shapeID") %>%
  left_join(adm2_ht_storms, by = c("shapeID" = "ADM2_id")) %>%
  mutate(log_exposure = log10(total_person_day_exposure + 0.1)) %>%
  st_transform(4326)

# ========================================
# 5. ### OPTIMIZATION ### KEEP ONLY NECESSARY COLUMNS
# ========================================

cat("Selecting only the columns needed for the Shiny app...\n")

# This is the most important step for reducing file size.
app_tc_exp <- adm2_tc_exp_wgs84 %>%
  select(
    shapeName, country, shapeGroup,
    total_person_day_exposure, log_exposure, total_population,
    pop_wt_grdi, quartile,
    contributing_storms
  )

app_hurr_exp <- adm2_hurr_exp_wgs84 %>%
  select(
    shapeName, country, shapeGroup,
    total_person_day_exposure, log_exposure, total_population,
    pop_wt_grdi, quartile,
    contributing_storms
  )

app_tc_tracks <- tc_tracks_2024 %>%
  select(unique_identifier, storm_id, date, latitude, longitude, wind, month)

app_ht_tracks <- ht_tracks_2024 %>%
  select(unique_identifier, storm_id, date, latitude, longitude, wind, month)

# ========================================
# 6. SAVE PROCESSED DATA
# ========================================

output_dir <- "./shiny_app/processed_data_v4/" # Adjusted path
if(!dir.exists(output_dir)) {
  dir.create(output_dir)
}

cat("Saving OPTIMIZED data as RDS files...\n")
saveRDS(app_tc_exp, file.path(output_dir, "adm2_tc_exp_wgs84.rds"))
saveRDS(app_hurr_exp, file.path(output_dir, "adm2_hurr_exp_wgs84.rds"))
saveRDS(adm0_boundaries_wgs84, file.path(output_dir, "adm0_boundaries_wgs84.rds")) # Already simplified
saveRDS(app_tc_tracks, file.path(output_dir, "tc_tracks_2024.rds"))
saveRDS(app_ht_tracks, file.path(output_dir, "ht_tracks_2024.rds"))

# --- Create and Save Summary Stats ---
summary_stats <- list(
  tc_storms = n_distinct(app_tc_tracks$unique_identifier),
  hurr_storms = n_distinct(app_ht_tracks$unique_identifier),
  data_processed_date = Sys.Date()
)
saveRDS(summary_stats, file.path(output_dir, "summary_stats.rds"))

# ========================================
# 7. PRINT SUMMARY
# ========================================
cat("\n=== Data Preprocessing Complete ===\n")
cat("Optimized files saved in 'processed_data_v3' directory:\n")
cat("- adm2_tc_exp_wgs84.rds (", round(file.size(file.path(output_dir, "adm2_tc_exp_wgs84.rds"))/1024^2, 2), " MB)\n", sep="")
cat("- adm2_hurr_exp_wgs84.rds (", round(file.size(file.path(output_dir, "adm2_hurr_exp_wgs84.rds"))/1024^2, 2), " MB)\n", sep="")
cat("- adm0_boundaries_wgs84.rds (", round(file.size(file.path(output_dir, "adm0_boundaries_wgs84.rds"))/1024^2, 2), " MB)\n", sep="")
cat("- tc_tracks_2024.rds (", round(file.size(file.path(output_dir, "tc_tracks_2024.rds"))/1024^2, 2), " MB)\n", sep="")
cat("- ht_tracks_2024.rds (", round(file.size(file.path(output_dir, "ht_tracks_2024.rds"))/1024^2, 2), " MB)\n", sep="")

