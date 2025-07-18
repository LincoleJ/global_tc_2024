# total county-event exposure for study period (1980-2024)
rm(list=ls())

### Load person-day exposure
## Tropical cyclones
all_pday_tc_exp <- list()
for (year in 1980:2024) {
  # load person-day exposures
  pday_tc_exp = readr::read_csv(paste0("./01_data/processed_pday_exp_data/pday_tc_exp_",
                                       year, ".csv")) %>% 
    mutate(year = year)
  all_pday_tc_exp[[as.character(year)]] <- pday_tc_exp
}
all_pday_tc_exp <- bind_rows(all_pday_tc_exp)[, -1] %>%
  filter(if_all(everything(), ~ !is.na(.))) # if NA, assumes population = 0, can exclude from analysis

## Hurricanes
all_pday_hurr_exp = list()
for (year in 1980:2024) {
  # load person-day exposures
  pday_hurr_exp = readr::read_csv(paste0("./01_data/processed_pday_exp_data/pday_hurr_exp_",
                                         year, ".csv")) %>% 
    mutate(year = year)
  all_pday_hurr_exp[[as.character(year)]] <- pday_hurr_exp
}
all_pday_hurr_exp <- bind_rows(all_pday_hurr_exp)[, -1] %>%
  filter(if_all(everything(), ~ !is.na(.)))

# 1a. Summarize total person-day exposure for world
total_tc_exp_days = all_pday_tc_exp %>%
  group_by(ADM2_id) %>%
  summarise(sum_exp_days = sum(total_exposure_days))

total_hurr_exp_days = all_pday_hurr_exp %>%
  group_by(ADM2_id) %>%
  summarise(sum_exp_days = sum(total_exposure_days))

# Administrative unit boundaries
adm2_boundaries = sf::read_sf("./01_data/adm_boundaries/geoBoundariesCGAZ_ADM2.geojson")
adm0_boundaries = sf::read_sf("./01_data/adm_boundaries/geoBoundariesCGAZ_ADM0.geojson") %>%
  filter(shapeGroup != "ATA")

adm2_nz_tc_exp = merge(adm2_boundaries, total_tc_exp_days, 
                       by.x = "shapeID", by.y = "ADM2_id") %>% 
  filter(sum_exp_days != 0)
adm2_nz_hurr_exp = merge(adm2_boundaries, total_hurr_exp_days, 
                         by.x = "shapeID", by.y = "ADM2_id") %>% 
  filter(sum_exp_days != 0)


# ensure same range
global_fill_range <- range(c(adm2_nz_tc_exp$sum_exp_days, 
                             adm2_nz_hurr_exp$sum_exp_days))
x = ggplot() +
  geom_sf(data = adm0_boundaries, fill = "grey86", color = "black", linewidth = 0.1) +
  geom_sf(data = adm2_nz_tc_exp, aes(fill = sum_exp_days), 
          color = NA) +
  scale_fill_gradient(low = "lightblue",
                      high = "navyblue",
                      name = "county-day exposure",
                      breaks = c(1, 14, 27, 40, 53, 66, 78),
                      labels = c(1, 14, 27, 40, 53, 66, 78)) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal")
ggsave("./03_output/global_tc_exp_days_map.jpg", x, 
       dpi = 1000, width = 12, height = 6)

y = ggplot() +
  geom_sf(data = adm0_boundaries, fill = "grey86", color = "black", linewidth = 0.1) +
  geom_sf(data = adm2_nz_hurr_exp, aes(fill = sum_exp_days), 
          color = NA) +
  scale_fill_gradient(low = "salmon1",
                      high = "darkred",
                      name = "county-day exposure",
                      breaks = c(1, 6, 11, 16, 22),
                      labels = c(1, 6, 11, 16, 22)) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal")
ggsave("./03_output/global_hurr_exp_days_map.jpg", y, 
       dpi = 1000, width = 12, height = 6)
