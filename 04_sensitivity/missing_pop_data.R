library(dplyr)
feather_pop_2020 = arrow::read_feather(paste0("./01_data/1a_raw/pop_by_adm2/adm2_pop_2020.feather"))
csv_pop_2020 = readr::read_csv("./01_data/1a_raw/pop_data_by_adm2/pop_2020_by_adm2.csv")[, -1]

length(unique(feather_pop_2020$shapeID)) # 48877
length(unique(csv_pop_2020$shapeID)) # 48969

missed_feather_pop_2020 = anti_join(csv_pop_2020, feather_pop_2020, by = "shapeID") %>%
  left_join(adm2_key, by = "shapeID")

feather_pop_2010 = arrow::read_feather(paste0("./01_data/1a_raw/pop_by_adm2/adm2_pop_2010.feather"))
arrow::write_feather(missed_feather_pop_2020, "./04_sensitivity/pop_by_adm2_nas.feather")
