# Characterizing global tropical cyclone events of 2024

Work in progress by Lingke Jiang, Victoria D. Lynch, G. Brooke Anderson, Xiao Wu, Robbie M. Parks

## Project description

The dataset and code used for paper to characterize global tropical cyclone events of 2024.

## 1. Data

1a_raw: processed wind data by ADM2 from `global_tc_data`; hurricane tracks data from IBTrACS; population by ADM2 from GHSL; population weighted GRDI by ADM2

1b_intermediate: interpolated annual population by ADM2

1c_support: files used to help analysis (e.g. ADM boundaries from `geoboundaries.org`, definitions of WHO regions, etc.)

1d_summary: processed person-day exposure by ADM2

## 2. Code

2a_person_day_over_grdi: code for scatterplots of person-day exposure against GRDI (Fig. 1B, D)

2b_exp_over_time: code for figures that compare 2024 person-day exposure to historical records by country, WHO region, and worldwide (Fig. 2A, C)

2c_global_map_exp: code for figures that plot global map of person-day exposure with hurricane tracks that contribute to them (Fig. 1A, C)

2d_rank_storms: code for tables that rank the most exposed ADM2s and the storms that contributed to the most exposures (Fig. 2B-C, 2E-F)

2e_pday_exp_year_reg: code for regression model between person-day exposure per capita and years

2f_all_study_period_exp_map: code for global map of person-day exposure totaled over time (1980-2024)

## 3. Output

3a_pday_exp_vs_grdi: Fig. 1B, D

3b_pday_exp_rank: Fig. 2B-C, 2E-F

3c_pday_exp_map: Fig. 1A, C

3d_pday_exp_through_time: Fig. 2A, C

3e_pday_exp_through_time_map: global maps of person-day exposure totaled over 1980-2024

3f_per_capita_pday_maps: regression of per-capita person-day exposure over year

3g_who_region_map: illustrative map of WHO regions

## 4. Sensitivity Analysis

Supplementary sensitivity analysis

## Directory structure

``` md
├── 01_data
│   ├── 1a_raw
│   │   ├── global_hurr_dat
│   │   ├── IBTrACS_raw_data
│   │   ├── pop_by_adm2
│   │   ├── pop_wt_grdi_data
│   ├── 1b_intermediate
│   │   └── pop_by_adm2_interpolated
│   ├── 1c_support
│   │   └── adm2_boundaries
│   │   └── who-regions
│   ├── 1d_summary
│   │   └── processed_pday_exp_data
│   │   └── bbox_by_region.csv
│   │   └── tc_grdi.csv
│   │   └── hurr_grdi.csv
│   │   └── adm0_boundaries.rds
│   │   └── adm2_boundaries.rds
│   │   └── adm2_lookup.rds
│   │   └── adm2_who_key.rds
├── 02_code
│   ├── 20_setup
│   │   └── 00_setup_cache.R
│   │   └── 01_helper_functions.R
│   │   └── 02_interpolate_pop_by_adm2.R
│   │   └── 03_process_pday_exp.R
│   │   └── 04_get_region_coord.R
│   ├── 2a_person_day_over_grdi.R
│   ├── 2b_exp_over_time.R
│   ├── 2c_global_map_exp.R
│   ├── 2d_rank_storms.R
│   ├── 2e_pday_exp_year_reg.R
│   ├── 2f_all_study_period_exp_map.R
│   ├── 2i_temporal_trend_by_region.R
├── 03_output
│   ├── 3a_pday_exp_vs_grdi
│   │   ├── pday_exp_vs_grdi.jpg
│   ├── 3b_pday_exp_rank
│   │   ├── ranked_storm.docx
│   │   ├── Most Exposed ADM2.docx
│   ├── 3c_pday_exp_map
│   │   ├── global_tracks_map.jpg
│   └── 3d_pday_exp_through_years
│   │   ├── exp_all.jpg
│   └── 3e_pday_exp_through_years_map
│   │   ├── global_tc_exp_days_map.jpg
│   │   ├── global_hurr_exp_days_map.jpg
│   └── 3f_per_capita_pday_maps
│   │   ├── per_capita_pday_tc_exp.png
│   │   ├── per_capita_pday_hurr_exp.png
│   └── 3g_who_region_maps
│   │   ├── who_region_map.jpg
├── 04_sensitivity
├── shiny_app
│   ├── preprocess_data.R
│   ├── app_optimized.R
│   ├── processed_data
│   ├── deploy.R
├── README.md
```

## Data Availability
The data used in this study were created from the following datasets. Tropical cyclone best track data during 1980-2024 are freely available at [IBTrACS](https://www.ncei.noaa.gov/products/international-best-track-archive); global gridded population data at 1km resolution during 1980-2020 are available quinquennially at [GHSL](https://human-settlement.emergency.copernicus.eu/download.php?ds=pop); global gridded deprivation index data at 1km resolution in 2020 are available at [EarthData](https://www.earthdata.nasa.gov/data/catalog/sedac-ciesin-sedac-pmp-grdi-2010-2020-1.00). 

