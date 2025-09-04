# simple regression between *per-capital* global person-day exposure and year (1980-2024)
rm(list=ls())

# 0a. Load packages
library(dplyr)
library(scales)
library(ggplot2)
library(patchwork)
library(forcats)

# 0b. Load data
source("./02_code/20_setup/01_helper_functions.R")
# Load WHO regions
adm2_key <- load_adm2_who_mapping()

# Load person-day exposures
all_pday_tc_exp <- load_person_day_exposures(type = "tc")
all_pday_hurr_exp <- load_person_day_exposures(type = "hurr")

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
         per_cap_exp = sum_pday_exp / pop_sum)

per_capita_hurr_model = lm(per_cap_exp ~ year, per_capita_hurr_exp)
ggplot(per_capita_hurr_exp, aes(x = year, y = per_cap_exp)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


# 2. calculate the total number population exposed in 2024
tc_2024_exp = readr::read_csv(paste0("./01_data/1d_summary/processed_pday_exp_data/pday_tc_exp_2024.csv"))[, -1] %>%
  na.omit()
sum(tc_2024_exp$total_population)

hurr_2024_exp = readr::read_csv(paste0("./01_data/1d_summary/processed_pday_exp_data/pday_hurr_exp_2024.csv"))[, -1] %>%
  na.omit()
sum(hurr_2024_exp$total_population)

pop_2020_by_adm2 = readr::read_csv("./01_data/1a_raw/pop_data_by_adm2/pop_2020_by_adm2.csv")
sum(pop_2020_by_adm2$adm_pop)

sum(tc_2024_exp$total_population) / sum(pop_2020_by_adm2$adm_pop)

# 3.  How many percentage of exposed population in GRDI

# Load data on GRDI
grdi_dat = arrow::read_feather("./01_data/1a_raw/pop_wt_grdi_data/pop_wt_grdi_2020.feather") 
tc_grdi = readr::read_csv("./01_data/1d_summary/tc_grdi.csv")[, -1] # GRDI of ADM2 units exposed to TC
hurr_grdi = readr::read_csv("./01_data/1d_summary/hurr_grdi.csv")[, -1] # GRDI of ADM2 units exposed to Hurricanes

most_deprived_tc = tc_grdi %>% filter(pop_wt_grdi < quantile(grdi_dat$pop_wt_grdi, probs = 0.25))
sum(most_deprived_tc$total_population) / sum(tc_grdi$total_population)

most_deprived_hurr = hurr_grdi %>% filter(pop_wt_grdi < quantile(grdi_dat$pop_wt_grdi, probs = 0.25)) 
sum(most_deprived_hurr$total_population) / sum(hurr_grdi$total_population)