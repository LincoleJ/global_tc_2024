# generate global map of total person-day exposure 
rm(list=ls())

# 0a. Load packages
library(ggplot2)
library(dplyr)
library(sf)
library(scales)
library(tidyverse)
library(viridis)
library(patchwork)
library(lubridate)

# 0b. Load data
source("./02_code/20_setup/01_helper_functions.R")
# storm track data
all_storms = readr::read_csv("./01_data/1a_raw/IBTrACS_raw_data/ibtracs.last3years.list.v04r01.csv")
hurr_tracks_2024 = data.frame(unique_identifier = all_storms$SID,
                              storm_id = all_storms$NAME,
                              usa_atcf_id = all_storms$USA_ATCF_ID,
                              date = all_storms$ISO_TIME,
                              latitude = all_storms$USA_LAT,
                              longitude = all_storms$USA_LON,
                              wind = all_storms$USA_WIND) %>%
  na.omit() %>%
  dplyr::mutate(latitude = as.numeric(latitude),
                longitude = as.numeric(longitude),
                wind = as.numeric(wind)) %>%
  dplyr::mutate(storm_id = paste0(tools::toTitleCase(tolower(storm_id)),
                                  "-", substr(date, 1, 4))) %>%
  mutate(date = format(date, "%Y%m%d%H%M")) %>% 
  filter(as.numeric(substr(date, 1, 4)) == 2024) %>%
  mutate(month = as.numeric(substr(date, 5, 6)))

# administrative unit boundaries
adm2_boundaries = load_adm2_boundaries()
adm0_boundaries = load_adm0_boundaries()

# person-day exposure
pday_tc_2024 = readr::read_csv("./01_data/1d_summary/processed_pday_exp_data/pday_tc_exp_2024.csv")[, -1]
pday_hurr_2024 = readr::read_csv("./01_data/1d_summary/processed_pday_exp_data/pday_hurr_exp_2024.csv")[ -1]

# bbox by WHO region
bbox_by_region = readr::read_csv("./01_data/1d_summary/bbox_by_region.csv")[, -1]

# 0c. Merge exposure data with ADM2 boundary and filter non-zero exposures
adm2_nz_tc_exp = merge(adm2_boundaries, pday_tc_2024, by.x = "shapeID", by.y = "ADM2_id") %>% 
  filter(total_person_day_exposure != 0)
adm2_nz_hurr_exp = merge(adm2_boundaries, pday_hurr_2024, by.x = "shapeID", by.y = "ADM2_id") %>% 
  filter(total_person_day_exposure != 0)

# 0d. Filter hurricane tracks that meet 1) tropical cyclones and 2) hurricane/typhoon 
# threshold
global_storm_winds_2024 = readRDS("./01_data/1a_raw/global_hurr_dat/global_storm_winds_2024.csv")
tc_track_names = global_storm_winds_2024 %>% 
  filter(vmax_sust >= 17.4911)
tc_tracks_2024 = subset(hurr_tracks_2024, unique_identifier %in% tc_track_names$sid)
ht_track_names = global_storm_winds_2024 %>% filter(vmax_sust >= 32.9244) %>% 
  pull(sid) %>%
  unique()
ht_tracks_2024 = hurr_tracks_2024 %>% filter(unique_identifier %in% ht_track_names)

# 1a. Map of total global person-day TC exposure by ADM2
# ensure same range
global_fill_range <- range(c(adm2_nz_tc_exp$total_person_day_exposure, 
                             adm2_nz_hurr_exp$total_person_day_exposure))
x = ggplot() +
  geom_sf(data = adm0_boundaries, fill = "grey97", color = "black", linewidth = 0.1) +
  geom_sf(data = adm2_nz_tc_exp, aes(fill = total_person_day_exposure), 
          color = NA, show.legend = FALSE) +
  scale_fill_viridis(trans = "log10",
                     option = "rocket",
                     direction = -1,
                     limits = global_fill_range,
                     breaks = c(0.1, 1, 10, 100, 1000, 10000, 100000, 1e6, 1e7),
                     labels = scales::label_number(accuracy = 1),
                     name = "Person-days exposure") +
  geom_path(data = tc_tracks_2024,
            aes(x = longitude, y = latitude, group = unique_identifier, color = month),
            linewidth = 0.15, 
            show.legend = FALSE) +
  geom_rect(data = bbox_by_region,
            aes(xmin = xmin-3, xmax = xmax+3, ymin = ymin-3, ymax = ymax+3),
            fill = NA, color = "black", linewidth = 0.3) +
  geom_text(data = bbox_by_region,
            aes(x = xmin-3+2, y = ymax+3-2, 
                label = c("c", "a", "b", "d", "e")),
            size = 2.5,
            family = "Century",
            fontface = "bold",
            color = "black") +
  scale_color_viridis_c(option = "mako",
                        #    direction = -1,
                        name = "Month", 
                        breaks = 1:12, 
                        limits = c(1, 12),
                        labels = month.abb) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) + 
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.25, barwidth = 10),
         color = guide_colorbar(title.position = "top", title.hjust = 0.25, barwidth = 10))

ggsave("./03_output/3c_pday_exp_map/global_pday_tc_map.jpg", x, 
       dpi = 1000, width = 12, height = 6)

# 1b. Map of total global person-day hurricane expoures by ADM2
y = ggplot() +
  geom_sf(data = adm0_boundaries, fill = "grey97", color = "black", linewidth = 0.1) +
  geom_sf(data = adm2_nz_hurr_exp, 
          aes(fill = total_person_day_exposure), 
          color = NA) +
  scale_fill_viridis(trans = "log10",
                     option = "rocket",
                     direction = -1,
                     breaks = c(0.1, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000),
                     limits = global_fill_range,
                     labels = scales::label_number(accuracy = 1),
                     name = "Person-days exposure") +
  geom_path(data = ht_tracks_2024,
            aes(x = longitude, y = latitude, group = unique_identifier, color = month),
            linewidth = 0.15) +
  geom_rect(data = bbox_by_region %>% filter(who_region != "Europe",
                                             who_region != "South-East Asia"),
            aes(xmin = xmin-3, xmax = xmax+3, ymin = ymin-3, ymax = ymax+3),
            fill = NA, color = "black", linewidth = 0.3) +
  geom_text(data = bbox_by_region %>% filter(who_region != "Europe",
                                             who_region != "South-East Asia"),
            aes(x = xmin-3+2, y = ymax+3-2, 
                label = c("c", "a", "e")),
            size = 2.5,
            family = "Century",
            fontface = "bold",
            color = "black") +
  scale_color_viridis_c(option = "mako", 
                        #  direction = -1,
                        name = "Month", 
                        breaks = 1:12, 
                        limits = c(1, 12),
                        labels = month.abb) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) + 
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.25, barwidth = 10),
         color = guide_colorbar(title.position = "top", title.hjust = 0.25, barwidth = 10))
ggsave("./03_output/3c_pday_exp_map/global_pday_hurr_map.jpg", y, 
       dpi = 1000, width = 12, height = 6)

# save as one plot 
tc_ht_map = (x / y) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.text = element_text(angle = 45, hjust = 1))
ggsave("./03_output/3c_pday_exp_map/global_tracks_map.jpg", tc_ht_map,
       dpi = 1000, width = 12, height = 12)