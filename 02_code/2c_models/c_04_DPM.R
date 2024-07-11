rm(list = ls())

#0a.Declare root directory, folder location and load essential stuff
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'create_folder_structure.R'))
source(paste0(functions.folder,'script_initiate.R'))

#1a.Load data
flood_fips <- read_csv(paste0(exposure.data.folder, "gfd_with_flood_type.csv")) %>% 
  janitor::clean_names() %>%
  mutate(fips = sprintf("%05d", geoid)) %>%
  filter(ghsl_popexp2015 > 0) %>% 
  filter(!state %in% c("AK", "PR", "VI")) %>%
  dplyr::select(fips) %>% distinct()

pop_weights <- read_csv(paste0(support.data.folder, "population_weights.csv")) %>% dplyr::select(-pop)
pop_data <- read_csv(paste0(support.data.folder, "cdc_population_monthly_infer.csv")) %>% 
  left_join(pop_weights)
mort_data <- read_csv(paste0(outcome.data.folder, "tidy_mortality_data.csv"))

health_outcome_data <- left_join(mort_data, pop_data) 
health_outcome_flood_only <- left_join(flood_fips,health_outcome_data)


#2.Mortality by season, sex, and age group 
test <- health_outcome_flood_only %>% 
  rename("cause" = "group") %>% 
  group_by(year, month, cause, age, sex) %>% 
  summarise(deaths = sum(deaths, na.rm = T), pop = sum(pop, na.rm = T)) %>% 
  left_join(pop_weights) %>% 
  mutate(asdr = (deaths/pop)*1000000*weight) %>% 
  group_by(year, month, cause, sex) %>% 
  summarise(asdr = sum(asdr)) %>% 
  mutate(mo_days = ifelse(month %in% c(2), 28,
                          ifelse(month %in% c(1,3,5,7,8,10,12), 31,
                                 ifelse(month %in% c(4,6,9,11),30,0)))) %>% 
  mutate(stnd_deaths = (31/mo_days*asdr)) %>% 
  mutate(sex = ifelse(sex %in% c(2), 0, 1))

#Monthly ASDR for each outcome in 2018
test2 <- test |> filter(year == 2018) |> filter(!cause %in% c('Other', 'NA')) |> 
  group_by(year, month,cause,mo_days) |> summarize(asdr = sum(asdr)/2) |> 
  group_by(year, cause) |> summarize(asdr = sum(asdr)/12)
  
head(test2)
us_pop_2018 <- 326800000

sanity_check <- (test2$asdr[1]*12*us_pop_2018)/1000000 
#505,436 cancer deaths in 2018 in US counties that experienced a flood during study period; estimated 609,640 cancer deaths in US in 2018 per American Cancer Society 
#82.9% of all cancer deaths in 2018 in US are included in analysis; 77.8% of all deaths in US during study period are included in analysis
#roughtly equivalent 

#3.Effect estimates
eff_est <- read_csv(paste0(model.output.folder, 'full_flood_cause_results.csv')) |> 
  filter(str_detect(rowname, "lag_")) %>% 
  filter(group == 'overall') |> 
  mutate(across(all_of("rowname"), str_remove,pattern = "\\...*"))

#This is DPM for each lag
test3 <- left_join(test2, eff_est) |> 
  mutate(attr_dpm = asdr*mean, 
         attr_low = asdr*`0.025quant`, 
         attr_high = asdr*`0.975quant`) |> ungroup() |> 
  dplyr::select(c(cause,rowname,threshold,flood_cat,attr_dpm, attr_low, attr_high)) |> 
  mutate_at(5:7, round,2) |> 
  unite('t1', sep = ' (',attr_dpm, attr_low) |> 
  unite('t2', sep = ', ', t1, attr_high) |> 
  mutate('t3' = ')') |> 
  unite('DPM (95% CrI)', sep = '',t2, t3) |> 
  spread(flood_cat, 'DPM (95% CrI)') |> 
  mutate(rowname = case_when(rowname == 'lag_0' ~ '0', 
                             rowname == 'lag_1' ~ '1', 
                             rowname == 'lag_2' ~ '2', 
                             rowname == 'lag_3' ~ '3')) |> 
  filter(threshold != 'any') |> 
  mutate(threshold = case_when(threshold == '1_pert' ~ 'Mild',
                               threshold == '25_pert' ~ 'Moderate', 
                               threshold == '50_pert' ~ 'Severe', 
                               threshold == '75_pert' ~ 'Very severe'
                               ))
  


test3 |> write_csv(paste0(tables.folder, "attr_table.csv"))


