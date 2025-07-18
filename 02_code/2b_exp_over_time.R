# generate plots of historical person-day exposures from 1980-2024
rm(list=ls())

# 0a. Load packages
library(dplyr)
library(scales)
library(ggplot2)
library(patchwork)
library(forcats)

# 0b. Load data
## WHO region
who_key = readr::read_csv("./01_data/1c_support/who-regions/who-regions.csv") %>%
  mutate(`World regions according to WHO` = 
           stringr::str_remove(`World regions according to WHO`, " \\(WHO\\)"))
colnames(who_key) = c("country", "ctry_code", "year", "who_region")
admin2_units = sf::read_sf("./01_data/1c_support/adm_boundaries/geoBoundariesCGAZ_ADM2.geojson")
adm2_key = data.frame(shapeID = admin2_units$shapeID,
                      ctry_code = admin2_units$shapeGroup,
                      shapeName = admin2_units$shapeName) %>%
  distinct()
adm2_key = left_join(adm2_key, who_key, by = c("ctry_code")) %>% select(-year)

### NAs in adm2_key
adm2_key_nas = adm2_key[!complete.cases(adm2_key), ]

# fix NA's
## Greenland assumed Europe, Taiwan assumed western pacific
adm2_key = adm2_key %>% 
  mutate(who_region = case_when(ctry_code == "TWN" ~ "Western Pacific",
                                ctry_code == "VCT" ~ "Americas",
                                ctry_code == "GRL" | ctry_code == "XKX" | ctry_code == "LIE" ~ "Europe",
                                country == "Gaza Strip" | country == "West Bank" ~ "Eastern Mediterranean",
                                TRUE ~ who_region)) %>%
  mutate(country = case_when(ctry_code == "TWN" ~ "Taiwan",
                             ctry_code == "VCT" ~ "Saint Vincent and the Grenadines",
                             ctry_code == "GRL" ~ "Greenland",
                             ctry_code == "XKX" ~ "Republic of Kosovo",
                             ctry_code == "LIE" ~ "Liechtenstein",
                             ctry_code == "VAT" ~ "Vatican City State",
                             TRUE ~ country))

### Load person-day exposure
## Tropical cyclones
all_pday_tc_exp <- list()
for (year in 1980:2024) {
  # load person-day exposures
  pday_tc_exp = readr::read_csv(paste0("./01_data/1d_summary/processed_pday_exp_data/pday_tc_exp_",
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
  pday_hurr_exp = readr::read_csv(paste0("./01_data/1d_summary/processed_pday_exp_data/pday_hurr_exp_",
                                         year, ".csv")) %>% 
    mutate(year = year)
  all_pday_hurr_exp[[as.character(year)]] <- pday_hurr_exp
}
all_pday_hurr_exp <- bind_rows(all_pday_hurr_exp)[, -1] %>%
  filter(if_all(everything(), ~ !is.na(.)))

# 1a. Summarize total person-day exposure for world
total_pday_tc_exp = all_pday_tc_exp %>%
  group_by(year) %>%
  summarise(sum_pday_exp = sum(total_person_day_exposure))

total_pday_hurr_exp = all_pday_hurr_exp %>%
  group_by(year) %>%
  summarise(sum_pday_exp = sum(total_person_day_exposure))

# 1b. Summarize total person-day exposure for WHO regions
total_pday_tc_exp_who = left_join(all_pday_tc_exp, adm2_key, 
                                  by = c("ADM2_id" = "shapeID")) %>%
  group_by(year, who_region) %>% 
  summarise(sum_pday_tc_exp = sum(total_person_day_exposure), 
            .groups = "drop") 

total_pday_hurr_exp_who = left_join(all_pday_hurr_exp, adm2_key, 
                                    by = c("ADM2_id" = "shapeID")) %>%
  group_by(year, who_region) %>%
  summarise(sum_pday_hurr_exp = sum(total_person_day_exposure),
            .groups = "drop")

# 1c. Summarize total person-day exposure for each country exposed at 2024
total_pday_tc_exp_ctry = left_join(all_pday_tc_exp, adm2_key,
                                   by = c("ADM2_id" = "shapeID")) %>%
  group_by(year, country) %>%
  summarise(sum_pday_exp = sum(total_person_day_exposure),
            .groups = "drop") 

exp_tc_ctr_2024 = total_pday_tc_exp_ctry %>% filter(year == 2024)
total_pday_tc_exp_ctry = total_pday_tc_exp_ctry %>% 
  filter(country %in% exp_tc_ctr_2024$country)

total_pday_hurr_exp_ctry = left_join(all_pday_hurr_exp, adm2_key,
                                     by = c("ADM2_id" = "shapeID")) %>%
  group_by(year, country) %>%
  summarise(sum_pday_exp = sum(total_person_day_exposure),
            .groups = "drop")
exp_hurr_ctr_2024 = total_pday_hurr_exp_ctry %>% filter(year == 2024)
total_pday_hurr_exp_ctry = total_pday_hurr_exp_ctry %>%
  filter(country %in% exp_hurr_ctr_2024$country)

# 1d. Order by 2024 exposure
##** TC, WHO region
ord_tc_who = total_pday_tc_exp_who %>%
  filter(year == 2024) %>%
  select(who_region, sum_pday_tc_exp) %>%
  distinct() %>%
  arrange(sum_pday_tc_exp) %>%
  pull(who_region)
total_pday_tc_exp_who = total_pday_tc_exp_who %>%
  mutate(who_region = fct_relevel(who_region, c("Eastern Mediterranean",
                                                ord_tc_who)))
##** Hurricane, WHO region
ord_hurr_who = total_pday_hurr_exp_who %>%
  filter(year == 2024) %>%
  select(who_region, sum_pday_hurr_exp) %>% 
  distinct() %>%
  arrange(sum_pday_hurr_exp) %>%
  pull(who_region)
total_pday_hurr_exp_who = total_pday_hurr_exp_who %>%
  mutate(who_region = fct_relevel(who_region, c("Europe", 
                                                "Eastern Mediterranean",
                                                "South-East Asia",
                                                ord_hurr_who)))

##** TC, country
ord_tc_ctry = total_pday_tc_exp_ctry %>% 
  filter(year == 2024) %>%
  select(country, sum_pday_exp) %>%
  distinct() %>%
  arrange(sum_pday_exp) %>%
  pull(country)
total_pday_tc_exp_ctry = total_pday_tc_exp_ctry %>%
  mutate(country = fct_relevel(country, ord_tc_ctry))

##** Hurricane, country
ord_hurr_ctry = total_pday_hurr_exp_ctry %>%
  filter(year == 2024) %>%
  select(country, sum_pday_exp) %>%
  distinct() %>% 
  arrange(sum_pday_exp) %>%
  pull(country)
total_pday_hurr_exp_ctry = total_pday_hurr_exp_ctry %>%
  mutate(country = fct_relevel(country, ord_hurr_ctry))

#-----------------------------------------------------------------------------
# 2a. global plot
pday_tc_global_plot = ggplot(total_pday_tc_exp, aes(x = sum_pday_exp, 
                                                    y = as.factor(year))) +
  geom_jitter(data = filter(total_pday_tc_exp, year != 2024),
              aes(color = year),
              width = 0, height = 0.2, size = 2, shape = 16) +  # small vertical jitter
  geom_jitter(data = filter(total_pday_tc_exp, year == 2024),
              color = "#FF7F0E", width = 0, height = 0.2, size = 4, shape = 16) +
  scale_x_log10(limits = c(c(1, 1e9)), 
                breaks = c(1e3, 1e5, 1e7, 1e9),
                labels = label_comma()) +
  scale_color_gradient(low = "plum1", high = "purple4", 
                       labels = NULL, guide = "none") +
  labs(x = NULL,
       y = "Worldwide") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "none") + 
  coord_cartesian(clip = "off")

pday_hurr_global_plot = ggplot(total_pday_hurr_exp, aes(x = sum_pday_exp, 
                                                        y = as.factor(year))) +
  geom_point(data = filter(total_pday_hurr_exp, year != 2024),
             aes(color = year),
             size = 2, shape = 16) +
  geom_point(data = filter(total_pday_hurr_exp, year == 2024),
             color = "#FF7F0E", width = 0, height = 0.2, size = 4, shape = 16) +
  scale_x_log10(limits = c(c(1, 1e9)), 
                breaks = c(1e3, 1e6, 1e9),
                labels = label_comma()) +
  scale_color_gradient(low = "plum1", high = "purple4", 
                       labels = NULL, guide = "none") +
  labs(x = NULL,
       y = "Worldwide") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "none") + 
  coord_cartesian(clip = "off")

# 2b. plot according to WHO region
pday_tc_who_plot = ggplot(total_pday_tc_exp_who, 
                          aes(x = sum_pday_tc_exp, y = who_region)) +
  geom_jitter(data = filter(total_pday_tc_exp_who, year != 2024),
              aes(color = year),
              width = 0, height = 0.2, size = 2, alpha = 0.8) +
  geom_jitter(data = filter(total_pday_tc_exp_who, year == 2024),
              color = "#FF7F0E", 
              width = 0, height = 0.01, size = 4) +
  scale_x_log10(limits = c(c(1, 1e9)), 
                breaks = c(1e3, 1e6, 1e9),
                labels = label_comma()) +
  scale_color_gradient(low = "plum1", high = "purple4", guide = "none") +
  labs(x = NULL,
       y = "Region") +
  theme_minimal() + 
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  coord_cartesian(clip = "off")

pday_hurr_who_plot = ggplot(total_pday_hurr_exp_who, 
                            aes(x = sum_pday_hurr_exp, y = who_region)) +
  geom_jitter(data = filter(total_pday_hurr_exp_who, year != 2024),
              aes(color = year),
              width = 0, height = 0.2, size = 2, alpha = 0.8) +
  geom_jitter(data = filter(total_pday_hurr_exp_who, year == 2024),
              color = "#FF7F0E",
              width = 0, height = 0.01, size = 4) +
  scale_x_log10(limits = c(c(1, 1e9)), 
                breaks = c(1e3, 1e6, 1e9),
                labels = label_comma()) +
  scale_color_gradient(low = "plum1", high = "purple4", guide = "none") +
  labs(y = "Region",
       x = NULL) +
  theme_minimal() +
  theme(legend.position = "none",
      axis.text.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()) +
  coord_cartesian(clip = "off")

# 2c. Plot for each country
pday_tc_ctry_plot <- ggplot(total_pday_tc_exp_ctry, aes(x = sum_pday_exp, y = country)) +
  geom_jitter(data = filter(total_pday_tc_exp_ctry, year == 2024),
              color = "#FF7F0E",
              width = 0, height = 0.01, size = 4) +
  geom_jitter(data = filter(total_pday_tc_exp_ctry, year != 2024),
              aes(color = year),
              width = 0, height = 0.2, size = 2, alpha = 0.8) +
  geom_jitter(data = filter(total_pday_tc_exp_ctry, year == 2024),
              color = "#FF7F0E",
              width = 0, height = 0.001, size = 4) +
  scale_x_log10(limits = c(c(1, 1e9)), 
                breaks = c(1e3, 1e6, 1e9),,
                labels = label_comma()) +
  scale_color_gradient(low = "plum1", high = "purple4") +
  labs(x = "Person-day cyclonic storm exposure", 
       y = "Country / Territory",
       color = "Year") +
  theme_minimal() + 
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  coord_cartesian(clip = "off")

pday_hurr_ctry_plot <- ggplot(total_pday_hurr_exp_ctry, aes(x = sum_pday_exp, y = country)) +
  geom_jitter(data = filter(total_pday_hurr_exp_ctry, year != 2024),
              aes(color = year),
              width = 0, height = 0.2, size = 2, alpha = 0.8) +
  geom_jitter(data = filter(total_pday_hurr_exp_ctry, year == 2024),
              color = "#FF7F0E",
              width = 0, height = 0.01, size = 4) +
  scale_x_log10(limits = c(c(1, 1e9)), 
                breaks = c(1e3, 1e6, 1e9),
                labels = label_comma()) +
  scale_color_gradient(low = "plum1", high = "purple4", 
                       labels = NULL, guide = "none") +
  labs(x = "Person-day hurricane / typhoon exposure",
       y = "Country / Territory") +
  theme_minimal() + 
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  coord_cartesian(clip = "off")

###########------------------------------------------------------------------------
# 3. Putting each plot together
tc_plot = (pday_tc_global_plot / pday_tc_who_plot / pday_tc_ctry_plot) +
  plot_layout(heights = c(1, 3, 15))

ht_plot = (pday_hurr_global_plot / pday_hurr_who_plot / pday_hurr_ctry_plot) +
  plot_layout(heights = c(1, 3, 15))

fig3 = (pday_tc_global_plot / pday_tc_who_plot / pday_tc_ctry_plot /
          pday_hurr_global_plot / pday_hurr_who_plot / pday_hurr_ctry_plot) + 
  plot_layout(guides = "collect",
              ncol = 1, nrow = 6,
              heights = c(1, 3, 15, 1, 3, 15)) &
  theme(legend.position = "right")

ggsave("./03_output/3d_pday_exp_through_years/fig3.jpg", fig3, 
       width = 8, height = 16)
# pday_tc_plots = pday_tc_global_plot / pday_tc_who_plot / pday_tc_ctry_plot + 
#   plot_layout(heights = c(1, 3, 12)) + 
#   plot_annotation(title = "Global cyclonic storm exposure in person-day") &
#   theme(plot.title = element_text(hjust = 0.5))
# ggsave("./03_output/3d_pday_exp_through_years/tc_exp_all.jpg", 
#        pday_tc_plots, width = 14.6, height = 12)
# 
# # hurricanes
# pday_hurr_plots = pday_hurr_global_plot / pday_hurr_who_plot / pday_hurr_ctry_plot + 
#   plot_layout(heights = c(1, 3, 7)) 
# ggsave("./03_output/3d_pday_exp_through_years/hurr_exp_all.jpg", 
#        pday_hurr_plots, width = 14.6, height = 6.8)


