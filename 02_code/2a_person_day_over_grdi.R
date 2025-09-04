# generate log-person day exposure vs population-weighted GRDI, colored by 
rm(list = ls())

# 0a. Load packages
library(ggplot2)
library(dplyr)
library(patchwork)

# 0b. Load data / source code
source("./02_code/20_setup/01_helper_functions.R")
## population (from Tory's method)
pop_2020 = readr::read_csv("./01_data/1a_raw/pop_data_by_adm2/pop_2020_by_adm2.csv")[, -1] 
colnames(pop_2020) = c("shapeID", "pop")

## hurricane tracks
storm_track_dat = readRDS("./01_data/1a_raw/global_hurr_dat/global_storm_winds_2024.csv") %>%
  filter(vmax_sust >= 17.4911)
tc_exposures_2024 <- storm_track_dat
hurr_exposures_2024 = tc_exposures_2024 %>% filter(vmax_sust >= 32.9244)

# mutate person-day exposure
tc_exposures_2024 = tc_exposures_2024 %>% mutate(day = as.Date(date_time_max_wind))
hurr_exposures_2024 = hurr_exposures_2024 %>% mutate(day = as.Date(date_time_max_wind))

# GRDI
grdi_dat = arrow::read_feather("./01_data/1a_raw/pop_wt_grdi_data/pop_wt_grdi_2020.feather") 

# ADM2 reference key
adm2_key = load_adm2_who_mapping()

# 0c. merge everything
tc_exposures_2024 = left_join(tc_exposures_2024, adm2_key, by = c("ADM2_id" = "shapeID"))
tc_exposures_2024 = left_join(tc_exposures_2024, pop_2020, by = c("ADM2_id" = "shapeID"))
hurr_exposures_2024 = left_join(hurr_exposures_2024, adm2_key, by = c("ADM2_id" = "shapeID"))
hurr_exposures_2024 = left_join(hurr_exposures_2024, pop_2020, by = c("ADM2_id" = "shapeID"))

# 1. Calculate person-day exposures for each ADM2
person_day_tc_exposure <- tc_exposures_2024 %>%
  group_by(ADM2_id) %>%
  summarise(exposure_days = n_distinct(day), # count unique exposure days for each ADM2
            total_population = max(pop),  
            region = first(who_region)) %>% 
  mutate(person_day_exposure = exposure_days * total_population) 

person_day_hurr_exposure <- hurr_exposures_2024 %>%
  group_by(ADM2_id) %>%
  summarise(exposure_days = n_distinct(day),
            total_population = max(pop),
            region = first(who_region)) %>%
  mutate(person_day_exposure = exposure_days * total_population) 

# non-zero hurricane / TC person-day exposures
non_zero_person_day_tc_exposure = person_day_tc_exposure %>% filter(person_day_exposure != 0)
non_zero_person_day_hurr_exposure = person_day_hurr_exposure %>% filter(person_day_exposure != 0)

# merge tc data with GRDI
tc_grdi = merge(non_zero_person_day_tc_exposure, grdi_dat, 
                by.x = "ADM2_id", by.y = "shapeID")
hurr_grdi = merge(non_zero_person_day_hurr_exposure, grdi_dat,
                  by.x = "ADM2_id", by.y = "shapeID")
na_regions_tc = tc_grdi %>% filter(is.na(region)) 
tc_grdi = tc_grdi %>% 
  mutate(region = ifelse(country == "Taiwan, Province of China", "Western Pacific", region)) %>%
  mutate(region = ifelse(country == "Saint Vincent and the Grenadines", "Americas", region))
hurr_grdi = hurr_grdi %>% 
  mutate(region = ifelse(country == "Taiwan, Province of China", "Western Pacific", region)) %>%
  mutate(region = ifelse(country == "Saint Vincent and the Grenadines", "Americas", region))

# save for use later
write.csv(tc_grdi, "./01_data/1d_summary/tc_grdi.csv")
write.csv(hurr_grdi, "./01_data/1d_summary/hurr_grdi.csv")

# 2. Scatter plot for tropical cyclone exposures
# color is from official website
region_colors <- c("Africa" = "#6363c0",
                   "Americas" = "#f26829",
                   "South-East Asia" = "#40bf73",
                   "Europe" = "#008dc9",
                   "Eastern Mediterranean" = "#bd53bd",
                   "Western Pacific" = "#f4a81d")
fig1_a = ggplot(tc_grdi, aes(x = pop_wt_grdi, 
                             y = person_day_exposure, 
                             color = region)) +
  geom_point() +
  scale_y_log10(breaks = 10^seq(0, 8, by = 1),
                labels = scales::label_comma()) +
  scale_color_manual(values = region_colors) +
  labs(y = "Person-day cyclonic storm exposure",
       color = "Region") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.line = element_line(),
        legend.position = "none") +
  xlim(0, 100) +
  coord_cartesian(ylim = c(1, 1e8)) 
fig1_b = ggplot(hurr_grdi, aes(x = pop_wt_grdi, 
                               y = person_day_exposure, 
                               color = region)) +
  geom_point(show.legend = FALSE) +
  scale_y_log10(breaks = 10^seq(1, 8, by = 1),
                labels = scales::label_comma()) +
  labs(y = "Person-day hurricane / typhoon exposure",
       color = "Region") +
  scale_color_manual(values = region_colors) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(),
        legend.position = "none") +
  xlim(0, 100) +
  coord_cartesian(ylim = c(1, 1e8))


# save as one plot 
axis_title <- ggplot(data.frame(x = c(0, 1)), aes(x = x)) + 
  geom_blank() +
  theme_void() + 
  theme(axis.title.x = element_text()) + 
  labs(x = "Global Gridded Relative Deprivation Index")
fig2 = (fig1_a / fig1_b / axis_title) + 
  plot_layout(guides = "collect",
              heights = c(20, 20, 1)) &
  theme(legend.position = "right")
ggsave("./03_output/3a_pday_exp_vs_grdi/fig2.jpg", fig2,
       width = 8, height = 12)


# Appendix pinpointing Madagascar (for Tory)
fig1_a_mdg <- ggplot(tc_grdi, aes(x = pop_wt_grdi, 
                                  y = log_person_day_exposure)) +
  geom_point(data = subset(tc_grdi, country != "Madagascar"), 
             aes(color = region), alpha = 0.7) +
  geom_point(data = subset(tc_grdi, country == "Madagascar"), 
             aes(shape = "Madagascar"), 
             color = "red", size = 3) +
  scale_shape_manual(name = "", values = c("Madagascar" = 17)) +
  labs(x = "GRDI (mean)",
       y = "Log Person-Day Exposure",
       color = "WHO Region",
       title = "GRDI vs Person Day TC Exposure in 2024 by WHO Region") +
  theme_minimal() +
  xlim(0, 100) +
  ylim(-2, 17)