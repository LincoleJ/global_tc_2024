# simple regression between *per-capital* person-day exposure and year
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
## Greenland assumed Europe, Taiwan as well
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

tc_model <- lm(sum_pday_exp ~ year, data = total_pday_tc_exp)
ht_model <- lm(sum_pday_exp ~ year, data = total_pday_hurr_exp)

ggplot(total_pday_tc_exp, aes(x = year, y = sum_pday_exp)) +
  geom_point() +
  stat_smooth(method = lm) +
  labs(y = "total person-day exposure") +
  theme_minimal()

ggplot(total_pday_hurr_exp, aes(x = year, y = sum_pday_exp)) +
  geom_point() +
  stat_smooth(method = lm) +
  labs(y = "total person-day exposure") +
  theme_minimal()


# 1b. Summarize total population for world
pop_by_adm2 = list()
for (year in 1980:2020) {
  pop = readr::read_csv(paste0("./01_data/1b_intermediate/pop_by_adm2_interpolated/adm2_pop_", 
                               year, ".csv")) %>%
    summarise(pop_sum = sum(adm_pop)) %>%
    mutate(year = year)
  pop_by_adm2[[as.character(year)]] <- pop
}
pop_by_adm2 <- bind_rows(pop_by_adm2)

pop_2020 = pop_by_adm2 %>% filter(year == 2020) %>% pull(pop_sum)

# per capita tc exposure
per_capita_tc_exp = left_join(total_pday_tc_exp, pop_by_adm2, 
                              by = c("year" = "year")) %>%
  mutate(pop_sum = if_else(year > 2020, pop_2020, pop_sum),
         per_cap_exp = sum_pday_exp / pop_sum)

per_capita_tc_model = lm(per_cap_exp ~ year, per_capita_tc_exp)
ggplot(per_capita_tc_exp, aes(x = year, y = per_cap_exp)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# per capita hurricane exposure
per_capita_hurr_exp = left_join(total_pday_hurr_exp, pop_by_adm2,
                                by = c("year" = "year")) %>%
  mutate(pop_sum = if_else(year > 2020, pop_2020, pop_sum),
         per_cap_exp = sum(sum_pday_exp / pop_sum))

per_capita_hurr_model = lm(per_cap_exp ~ year, per_capita_hurr_exp)
ggplot(per_capita_hurr_exp, aes(x = year, y = per_cap_exp)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


# calculate the total number population exposed in 2024
tc_2024_exp = readr::read_csv(paste0("./01_data/1d_summary/processed_pday_exp_data/pday_tc_exp_2024.csv"))[, -1] %>%
  na.omit()
sum(tc_2024_exp$total_population)

hurr_2024_exp = readr::read_csv(paste0("./01_data/1d_summary/processed_pday_exp_data/pday_hurr_exp_2024.csv"))[, -1] %>%
  na.omit()
sum(hurr_2024_exp$total_population)

pop_2020_by_adm2 = readr::read_csv("./01_data/1a_raw/pop_data_by_adm2/pop_2020_by_adm2.csv")
sum(pop_2020_by_adm2$adm_pop)

sum(tc_2024_exp$total_population) / sum(pop_2020_by_adm2$adm_pop)
