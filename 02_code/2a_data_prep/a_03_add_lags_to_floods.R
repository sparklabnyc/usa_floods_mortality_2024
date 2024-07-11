rm(list = ls())
#0a.Declare root directory, folder location and load essential stuff
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'create_folder_structure.R'))
source(paste0(functions.folder,'script_initiate.R'))

#1a.Load flood and temperature data 
flood_data <- read_csv(paste0(exposure.data.folder, "flood_pop_data_by_thresh_type.csv")) |> #flood_pop_data_by_thresh_overall
  dplyr::select(c(year,month,geoid,dfo_began,dfo_id,state,flood_cat,n,expo_type,expo_threshold,flood_cat,flood_occur)) |> 
  mutate(fips = geoid)
temp_data <- read_csv(paste0(exposure.data.folder, "median_max_temp_fips_month_year.csv")) %>% 
  dplyr::select(-c(meteo_var, med_temp)) %>% 
  mutate_at(c("month"), as.numeric)

#1b.Set categories to iterate over
expo_types <- c("pop_expo")
expo_thresholds <- c("any", "1_pert","25_pert", "50_pert", "75_pert")

#1c.Generate dataset of all possible fips, year, month combinations 
year <- c(2001:2018)
month <- c(1:12)
fips <- c(unique(flood_data$fips))
flood_lag_grid <- expand_grid(year, month, fips)

#2.Run function to add lags
flood_with_lags <- create_flood_lags(expo_types, expo_thresholds)
colMeans(is.na(flood_with_lags))
n_distinct(flood_with_lags$dfo_id)

flood_data_with_lags_temp <- flood_with_lags %>% 
  mutate_at("fips", as.character) %>% 
  mutate_at("month", as.numeric) %>% 
  left_join(temp_data) %>% 
  filter(!is.na(max_mean_temp))
colMeans(is.na(flood_data_with_lags_temp))
n_distinct(flood_data_with_lags_temp$dfo_id)

flood_data_with_lags_temp %>% 
  write_csv(paste0(exposure.data.folder, "flood_pop_data_with_lags_.csv"))
