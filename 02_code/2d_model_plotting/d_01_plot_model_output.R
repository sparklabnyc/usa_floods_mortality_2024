rm(list = ls())
#0a.Declare root directory, folder location and load essential stuff
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'create_folder_structure.R'))
source(paste0(functions.folder,'script_initiate.R'))

#0a.Load datasets
model_results_flood_spf <- read_csv(paste0(model.output.folder, "model_results_flood_spf.csv"))
model_results_non_spf <- read_csv(paste0(model.output.folder, "model_results_non_spf.csv")) %>% 
  mutate(group = "over_non_spf") 

#1.Join datasets
overall_results <- bind_rows(model_results_flood_spf, model_results_non_spf)

#2.Load and prepare model output data
tidy_plot_data <- overall_results %>% 
  filter(str_detect(rowname, "lag_")) %>% 
  mutate(across(all_of("rowname"), str_remove,pattern = "\\...*")) %>% 
  mutate(plot_group = case_when(
    group == 'overall' ~ 'overall',
    group == 'over_non_spf' ~ 'overall_non_spf',
    group %in% c("1","2") ~ 'sex',
    group %in% c("64", "66") ~ 'age'
  )) %>% 
  #mutate(across(where(is.numeric), round,3)) %>%
  mutate(rowname = case_when(rowname == 'lag_0' ~ '0', 
                             rowname == 'lag_1' ~ '1', 
                             rowname == 'lag_2' ~ '2', 
                             rowname == 'lag_3' ~ '3')) %>% 
  mutate(flood_cat = case_when(flood_cat == "Snowmelt" ~ "Snowmelt",
                               flood_cat == "Heavy rain" ~ "Heavy rain",
                               flood_cat == "Tropical cyclones" ~ "Tropical cyclone",
                               flood_cat == "Ice jams and dam breaks" ~ "Ice jam or dam break",
                               flood_cat == "all_floods" ~ "All floods")) 

#2.Set labels, colors, etc. for figures
sex.labs <- c("Male", "Female")
causes.labs <- c("Injuries", "Cardiovascular diseases","Respiratory diseases","Cancers", "Infectious and\nparasitic diseases","Neuropsychiatric\nconditions" )
floodtypes.labs <- c("Heavy rain", "Snowmelt", "Tropical cyclone", "Ice jam or dam break")
age.labs <- c("Age 0-64", "Age 65+")
names(sex.labs) <- c("1", "2")
names(causes.labs) <- c("Injuries", "Cardiovascular diseases","Respiratory diseases","Cancers", "Infectious and parasitic diseases", "Neuropsychiatric conditions")
names(floodtypes.labs) <- c("Heavy rain", "Snowmelt", "Tropical cyclone", "Ice jam or dam break")
names(age.labs) <- c("64", "66")

#3a.Set plots to run
plot_groups <- c("overall_non_spf","overall","sex","age")
expo_types <- c("pop_expo")
expo_thresholds <- c("1_pert", "25_pert", "50_pert", "75_pert")

#3b.Make all severity plots
plot_all_flood_severity(plot_groups, expo_types)


