# total county-event exposure for study period (1980-2024)
rm(list=ls())

### Load person-day exposure
source("./02_code/20_setup/01_helper_functions.R")
## Tropical cyclones
all_pday_tc_exp = load_person_day_exposures(type = "tc")
## Hurricanes
all_pday_hurr_exp = load_person_day_exposures(type = "hurr")

# 1a. Summarize total person-day exposure for world
total_tc_exp_days = all_pday_tc_exp %>%
  group_by(ADM2_id) %>%
  summarise(sum_exp_days = sum(total_exposure_days))

total_hurr_exp_days = all_pday_hurr_exp %>%
  group_by(ADM2_id) %>%
  summarise(sum_exp_days = sum(total_exposure_days))

# Administrative unit boundaries
adm2_boundaries <- load_adm2_boundaries()
adm0_boundaries <- load_adm0_boundaries()

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
ggsave("./03_output/3e_pday_exp_through_years_map/global_tc_exp_days_map.jpg", x, 
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
ggsave("./03_output/3e_pday_exp_through_years_map/global_hurr_exp_days_map.jpg", y, 
       dpi = 1000, width = 12, height = 6)