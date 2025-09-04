# Helper functions
# Should be sourced an analysis scripts

# Constants
TROPICAL_STORM_THRESHOLD <- 17.4911  # m/s (34 knots)
HURRICANE_THRESHOLD <- 32.9244       # m/s (64 knots)

# Fast data loaders (using cached RDS files)
load_adm2_lookup <- function() {
  readRDS("./01_data/1d_summary/adm2_lookup.rds")
}

load_adm2_who_mapping <- function() {
  readRDS("./01_data/1d_summary/adm2_who_key.rds")
}

load_adm2_boundaries <- function() {
  readRDS("./01_data/1d_summary/adm2_boundaries.rds")
}

load_adm0_boundaries <- function() {
  readRDS("./01_data/1d_summary/adm0_boundaries.rds")
}

# Function to load person-day exposures
load_person_day_exposures <- function(start_year = 1980, end_year = 2024, type = "tc") {
  all_pday_exp <- list()
  file_prefix <- ifelse(type == "tc", "pday_tc_exp_", "pday_hurr_exp_")
  
  for (year in start_year:end_year) {
    pday_exp <- readr::read_csv(
      paste0("./01_data/1d_summary/processed_pday_exp_data/", 
             file_prefix, year, ".csv")
    )[, -1] %>% mutate(year = year)
    all_pday_exp[[as.character(year)]] <- pday_exp
  }
  
  bind_rows(all_pday_exp) %>%
    filter(if_all(everything(), ~ !is.na(.)))
}
