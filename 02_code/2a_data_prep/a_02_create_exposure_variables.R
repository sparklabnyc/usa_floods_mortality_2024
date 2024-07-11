rm(list = ls())

#0a.Declare root directory, folder location and load essential stuff
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'create_folder_structure.R'))
source(paste0(functions.folder,'script_initiate.R'))

#0b.Load datasets
gfd_data <- read_csv(paste0(exposure.data.folder, "gfd_with_flood_type.csv")) %>%
  janitor::clean_names() %>%
  dplyr::select(c(geoid,dfo_began,dfo_id,flood_days_max,flood_days_mean,ghsl_pop2015,
                  ghsl_popexp2015,state,year,month,flood_cat)) %>% distinct() %>%
  filter(!state %in% c("AK", "PR", "VI"))
  
#1.Create exposure thresholds by population (GHSL 2015)

flood_cats <- c("Heavy rain", "Snowmelt", "Tropical cyclone", "Ice jam or dam break")
expo_by_flood_cause <- create_exposure_quantile(flood_cats)

n_distinct(expo_by_flood_cause$dfo_id) #check that floods in AK and PR are removed
n_distinct(expo_by_flood_cause$geoid)

write_csv(expo_by_flood_cause, paste0(exposure.data.folder, "flood_pop_data_by_thresh_type.csv"))

expo_overall <- gfd_data %>% 
  filter(ghsl_popexp2015 > 0) %>% 
  dplyr::select(-flood_cat) |> 
  mutate(perc_pop_flood_2015 = ghsl_popexp2015/ghsl_pop2015, 
         quant_pop_flood = ntile(perc_pop_flood_2015, 4)) %>% 
  mutate(expo_type = "pop_expo") %>% 
  add_count(geoid,dfo_began,year,month,quant_pop_flood) %>% 
  mutate("any" = case_when(
    quant_pop_flood  > 0 ~ 1)) %>% 
  mutate("1_pert" = case_when(
    quant_pop_flood == 1 ~ 1)) %>% 
  mutate("25_pert" = case_when(
    quant_pop_flood == 2 ~ 1)) %>% 
  mutate("50_pert" = case_when(
    quant_pop_flood == 3 ~ 1)) %>% 
  mutate("75_pert" = case_when(
    quant_pop_flood == 4 ~ 1)) %>% 
  mutate_at(c(15:19), ~replace_na(.,0)) %>% 
  pivot_longer(cols = 15:19,
               names_to = "expo_threshold",
               values_to = "flood_occur") %>%  
  mutate(flood_cat = 'all_floods')
  
flood_data_by_threshold <- expo_overall %>% 
  dplyr::select(-c(quant_pop_flood))
n_distinct(flood_data_by_threshold$dfo_id) #check that floods in AK and PR are removed

write_csv(flood_data_by_threshold, paste0(exposure.data.folder, "flood_pop_data_by_thresh_overall.csv"))
