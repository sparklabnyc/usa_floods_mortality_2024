rm(list = ls())
#0a.Declare root directory, folder location and load essential stuff
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'create_folder_structure.R'))
source(paste0(functions.folder,'script_initiate.R'))

#1a.Load flood data
flood_data <- read_csv(paste0(exposure.data.folder, "flood_pop_data_with_lags_type.csv"), 
                       col_types = cols(stratum = readr::col_factor())) %>%  
  mutate(fips = as.character(fips)) 

#1b.Set categories for iteration 
causes <- c( "Cardiovascular diseases", "Cancers", "Respiratory diseases",
             "Injuries", "Neuropsychiatric conditions", "Infectious and parasitic diseases") 
groups <- c("overall", "1", "2", "64", "66")
expo_types <- c("pop_expo")
expo_thresholds <- c("any", '1_pert',"25_pert", "50_pert", "75_pert")
flood_types <- c("Heavy rain", "Snowmelt", "Tropical cyclones", "Ice jams and dam breaks") #all_floods

#2.Set values for model
hyper_value <- 0.001 #hyperparameter value
num_df <- 54 #number of degrees of freedom for year term

#3.Run model
model_results <- run_model_function(causes,groups,expo_types,expo_thresholds,flood_types) %>% 
  rownames_to_column()
view(model_results)

#model_results %>% write_csv(paste0(model.output.folder, "model_results_flood_spf.csv"))
