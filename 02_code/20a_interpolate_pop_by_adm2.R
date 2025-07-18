# interpolate population by ADM2
rm(list=ls())

# 0a. Load packages
library(dplyr)

# 0b. Functions
# read feather datasets
read_pop_by_adm2 = function(year) {
  pop = arrow::read_feather(paste0("./01_data/pop_by_adm2/adm2_pop_", year, ".feather"))
  return(pop)
}

# interpolate population for 
interpolating_pop = function(pop1, pop2, t) {
  colnames(pop1) = c("shapeID", "st_pop")
  colnames(pop2) = c("shapeID", "end_pop")
  interpolated_pop = merge(pop1, pop2, by = "shapeID") %>% 
    mutate(adm_pop = st_pop + ((end_pop - st_pop) / 5 * t)) %>% 
    select(-st_pop, -end_pop)
  return(interpolated_pop)
}


# 1a. process population for ADM2 for interpolated years
avbl_pop_yrs = seq(1980, 2020, 5)
n_pairs = length(avbl_pop_yrs) - 1

for (i in 1:n_pairs) {
  # read rasters
  st_yr = avbl_pop_yrs[i]
  end_yr = avbl_pop_yrs[i+1]
  st_yr_pop = read_pop_by_adm2(st_yr)
  end_yr_pop = read_pop_by_adm2(end_yr)
  
  # return processed & interpolated pop rasters
  for (t in 1:4) {
    pop = interpolating_pop(st_yr_pop, end_yr_pop, t)
    write.csv(pop, paste0("./01_data/pop_by_adm2_interpolated/adm2_pop_", st_yr + t,
                          ".csv"))
  }
}

# 1b. save original data into the folder for more convenient use
for (i in 1:length(avbl_pop_yrs)) {
  pop = read_pop_by_adm2(avbl_pop_yrs[i])
  write.csv(pop, paste0("./01_data/pop_by_adm2_interpolated/adm2_pop_", avbl_pop_yrs[i],
                        ".csv"))
}
