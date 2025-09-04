# plot of WHO region illustration
rm(list=ls())

# 0a. Load Packages
library(ggplot2)
library(dplyr)

# 0b. Load data
source("./02_code/20_setup/01_helper_functions.R")
adm0_boundaries <- load_adm0_boundaries()

who_key = readr::read_csv("./01_data/1c_support/who-regions/who-regions.csv") %>%
  mutate(`World regions according to WHO` = 
           stringr::str_remove(`World regions according to WHO`, " \\(WHO\\)"))
colnames(who_key) = c("country", "ctry_code", "year", "who_region")

who_region_cords = left_join(adm0_boundaries, who_key, 
                             by = c("shapeGroup" = "ctry_code")) %>%
  mutate(who_region = ifelse(shapeName == "Taiwan", 
                             "Western Pacific", who_region)) %>%
  mutate(who_region = ifelse(shapeName == "St Vincent & the Grenadines", 
                             "Americas", who_region)) %>%
  select(-country, -year)

who_region_cords = left_join(adm0_boundaries, who_key, 
                             by = c("shapeGroup" = "ctry_code")) %>%
  mutate(who_region = ifelse(shapeName == "Taiwan", 
                             "Western Pacific", who_region)) %>%
  mutate(who_region = ifelse(shapeName == "St Vincent & the Grenadines", 
                             "Americas", who_region)) %>%
  select(-country, -year)

#0c. Define color palette
region_colors <- c("Africa" = "#6363c0",
                   "Americas" = "#f26829",
                   "South-East Asia" = "#40bf73",
                   "Europe" = "#008dc9",
                   "Eastern Mediterranean" = "#bd53bd",
                   "Western Pacific" = "#f4a81d",
                   "Unknown" = "grey")

who_region_cords <- who_region_cords %>%
  mutate(who_region = if_else(is.na(who_region) | 
                                !(who_region %in% names(region_colors)),
                              "Unknown", who_region))
lvls <- c("Unknown", setdiff(names(region_colors), "Unknown"))

who_map <- ggplot(who_region_cords) +
  geom_sf(aes(fill = who_region), color = "white") +
  scale_fill_manual(values = region_colors,
                    breaks = lvls,
                    drop = FALSE,
                    name = "WHO region") +
  coord_sf(expand = FALSE) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave("./03_output/3g_who_region_map/who_region_map.jpg", 
       who_map, dpi = 1000, width = 12, height = 6)
