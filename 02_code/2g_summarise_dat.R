library(dplyr)

# load tc_grdi, hurr_grdi from 1d_summary
# load ADM2 related data from 1c_support

hurr_summary = hurr_grdi %>% 
  group_by(region) %>% 
  summarise(total_pday_exp = sum(person_day_exposure))

## Europe
euro_tc = tc_grdi %>% filter(region == "Europe") %>%
  left_join(admin2_units, by = c("ADM2_id" = "shapeID"))

## Africa
afri_tc = tc_grdi %>% filter(region == "Africa") %>%
  left_join(admin2_units, by = c("ADM2_id" = "shapeID"))
afri_hurr = hurr_grdi %>% filter(region == "Africa") %>%
  left_join(admin2_units, by = c("ADM2_id" = "shapeID"))

## Western Pacific
wp_tc = tc_grdi %>% filter(region == "Western Pacific") %>% 
  left_join(admin2_units, by = c("ADM2_id" = "shapeID"))
wp_hurr = hurr_grdi %>% filter(region == "Western Pacific") %>% 
  left_join(admin2_units, by = c("ADM2_id" = "shapeID"))

## rank person-day exposure temporally
total_pday_tc_exp_who %>% 
  filter(who_region == "Africa") %>%
  mutate(rank = min_rank(desc(sum_pday_tc_exp))) %>%
  arrange(rank) %>%
  View()

total_pday_hurr_exp_who %>% 
  filter(who_region == "Africa") %>%
  mutate(rank = min_rank(desc(sum_pday_hurr_exp))) %>%
  arrange(rank) %>%
  View()
