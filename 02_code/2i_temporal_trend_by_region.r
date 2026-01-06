# we make temporal line plots for person-days of exposure for time, then
# stratify by region.
rm(list=ls())

library(dplyr)
library(tidyverse)
library(cowplot)
library(patchwork)
source("./02_code/20_setup/01_helper_functions.R")

adm2_region_info <- load_adm2_who_mapping()

years <- 1980:2024

# Function to load and process person-day exposure data
load_pday_data <- function(years, exposure_type = "tc") {
  
  all_data <- map_dfr(years, function(yr) {
    file_path <- paste0("./01_data/1d_summary/processed_pday_exp_data/pday_", 
                        exposure_type, "_exp_", yr, ".csv")
    
    if (file.exists(file_path)) {
      df <- readr::read_csv(file_path, show_col_types = FALSE)[, -1] %>%
        mutate(year = yr)
      return(df)
    } else {
      message(paste("File not found:", file_path))
      return(NULL)
    }
  })
  
  return(all_data)
}

# Load TC and Hurricane data
tc_data <- load_pday_data(years, "tc")
hurr_data <- load_pday_data(years, "hurr")

# Join with region info and aggregate by region and year
process_regional_data <- function(pday_data, adm2_info) {
  
  # Join with region mapping using ADM2_id/shapeID
  regional_data <- pday_data %>%
    left_join(adm2_info, by = c("ADM2_id" = "shapeID")) %>%
    group_by(year, who_region) %>%
    summarise(
      total_person_day_exposure = sum(total_person_day_exposure, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Calculate global totals
  global_data <- pday_data %>%
    group_by(year) %>%
    summarise(
      total_person_day_exposure = sum(total_person_day_exposure, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(who_region = "Worldwide")
  
  # Combine regional and global
  combined <- bind_rows(regional_data, global_data)
  
  # Ensure all year-region combinations exist (fill with 0 if missing)
  all_regions <- c("Worldwide", "Americas", "Western Pacific", "South-East Asia", 
                   "Africa", "Europe", "Eastern Mediterranean")
  all_years <- min(pday_data$year):max(pday_data$year)
  
  complete_grid <- expand.grid(year = all_years, who_region = all_regions, 
                               stringsAsFactors = FALSE)
  
  combined <- complete_grid %>%
    left_join(combined, by = c("year", "who_region")) %>%
    mutate(total_person_day_exposure = replace_na(total_person_day_exposure, 0))
  
  return(combined)
}

# Process both datasets
tc_regional <- process_regional_data(tc_data, adm2_region_info) %>%
  mutate(exposure_type = "Tropical Cyclone")

hurr_regional <- process_regional_data(hurr_data, adm2_region_info) %>%
  mutate(exposure_type = "Hurricane")

# Combine for plotting
all_data <- bind_rows(tc_regional, hurr_regional)

# Define region order (Global first, then WHO regions)
region_order <- c("Worldwide", "Americas", "Western Pacific", "South-East Asia", 
                  "Africa", "Europe", "Eastern Mediterranean")

all_data <- all_data %>%
  mutate(who_region = factor(who_region, levels = region_order))

# Function to create individual panel plots
create_exposure_plot_clean <- function(data, region_name, exp_type, 
                                       is_bottom = FALSE) {
  
  plot_data <- data %>%
    filter(who_region == region_name, exposure_type == exp_type)
  
  p <- ggplot(plot_data, aes(x = year, y = total_person_day_exposure)) +
    geom_line() +
    geom_point(size = 1.2) +
    scale_x_continuous(breaks = seq(1980, 2020, by = 20)) +
    scale_y_continuous(labels = scales::comma) +
    labs(x = NULL, y = NULL) +
    theme_bw() +
    theme(
      plot.title = element_blank(),
      axis.text = element_text(size = 8),
      axis.title.x = element_text(size = 10),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),  
      plot.margin = margin(2, 5, 2, 5)
    )
  
  # Remove x-axis text if not bottom row
  if (!is_bottom) {
    p <- p + theme(axis.text.x = element_blank(),
                   axis.ticks.x = element_blank())
  }
  
  return(p)
}

# Create region label as a text grob (right-aligned for far left position)
create_region_label <- function(region_name) {
  ggdraw() + 
    draw_label(region_name, fontface = "bold", size = 10, hjust = 1, x = 0.9)
}

# Build each row: region label | TC y-label | TC plot | Hurr y-label | Hurricane plot
build_row <- function(region_name, is_bottom = FALSE) {
  tc_plot <- create_exposure_plot_clean(all_data, region_name, "Tropical Cyclone", is_bottom)
  hurr_plot <- create_exposure_plot_clean(all_data, region_name, "Hurricane", is_bottom)
  
  # Just the two plots side by side with a small gap
  plot_grid(tc_plot, hurr_plot, 
            ncol = 2, rel_widths = c(1, 1), align = "h")
}

# Create all rows
is_bottom_vec <- region_order == "Eastern Mediterranean"
all_rows <- map2(region_order, is_bottom_vec, build_row)

# Stack rows vertically
main_panel <- plot_grid(plotlist = all_rows, ncol = 1, align = "v")

# Create region labels stacked vertically (to go on far left)
region_labels <- map(region_order, create_region_label)
region_label_column <- plot_grid(plotlist = region_labels, ncol = 1, align = "v")

# Create y-axis labels (these will span all rows)
tc_y_label <- ggdraw() + 
  draw_label("Tropical cyclone-force person-day exposure", angle = 90, size = 10)
hurr_y_label <- ggdraw() + 
  draw_label("Hurricane-force person-day exposure", angle = 90, size = 10)

# Split main_panel into TC and Hurricane halves to insert y-labels
# Rebuild with y-labels integrated
build_row_with_ylabel <- function(region_name, is_bottom = FALSE) {
  tc_plot <- create_exposure_plot_clean(all_data, region_name, "Tropical Cyclone", is_bottom)
  hurr_plot <- create_exposure_plot_clean(all_data, region_name, "Hurricane", is_bottom)
  
  list(tc = tc_plot, hurr = hurr_plot)
}

all_plots <- map2(region_order, is_bottom_vec, build_row_with_ylabel)

# Stack TC plots
tc_column <- plot_grid(plotlist = map(all_plots, "tc"), ncol = 1, align = "v")

# Stack Hurricane plots  
hurr_column <- plot_grid(plotlist = map(all_plots, "hurr"), ncol = 1, align = "v")

# Combine: region labels | TC y-label | TC plots | Hurricane y-label | Hurricane plots
header_row <- plot_grid(
  NULL, NULL,
  ggdraw() + draw_label("Tropical cyclone", fontface = "bold", size = 11),
  NULL,
  ggdraw() + draw_label("Hurricane", fontface = "bold", size = 11),
  ncol = 5,
  rel_widths = c(0.12, 0.04, 0.4, 0.04, 0.4)
)

plot_area <- plot_grid(
  region_label_column, tc_y_label, tc_column, hurr_y_label, hurr_column,
  ncol = 5, 
  rel_widths = c(0.12, 0.04, 0.4, 0.04, 0.4),
  align = "h"
)

middle_section <- plot_grid(
  header_row, plot_area,
  ncol = 1,
  rel_heights = c(0.03, 1)
)

# Add bottom x-axis label
bottom_label <- plot_grid(
  NULL, NULL, 
  ggdraw() + draw_label("Year", size = 11),
  NULL,
  ggdraw() + draw_label("Year", size = 11),
  ncol = 5,
  rel_widths = c(0.12, 0.04, 0.4, 0.04, 0.4)
)

with_bottom <- plot_grid(middle_section, bottom_label,
                         ncol = 1, rel_heights = c(1, 0.03))

# Final assembly
final_plot <- plot_grid(title, with_bottom,
                        ncol = 1, rel_heights = c(0.04, 1, 0.025))

ggsave("./03_output/3d_pday_exp_through_years/line_plots.jpg", final_plot,
       height = 14, width = 14, dpi = 1000)
