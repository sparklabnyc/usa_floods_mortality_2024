rm(list = ls())
#0a.Declare root directory, folder location and load essential stuff
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'create_folder_structure.R'))
source(paste0(functions.folder,'script_initiate.R'))

#1a.Load data
pop_data <- read_csv(paste0(support.data.folder, "cdc_population_monthly_infer.csv")) 
mort_data <- read_csv(paste0(outcome.data.folder, "tidy_mortality_data.csv")) %>% 
  rename(cause = group )

#1b.Set groups
causes <- c("Infectious and parasitic diseases", "Cardiovascular diseases", "Cancers", "Respiratory diseases",
            "Other", "Injuries", "Neuropsychiatric conditions")
groups <- c("overall", "1", "2", "0", "5", "15", "25", "35", "45", "55", "65", "75", "85")
coarse_groups <- c("64","66")

#2.Create datasets by cause and group; only need to run this once to get .rds output (saved in 1b_outcome_data)
for (subcauses in causes){
  
  print(subcauses)
  data <-  create_subcause_datasets(subcauses)
  
  for (subgroups in groups){
    
    print(subgroups)
    create_subgroup_datasets(subgroups)
  }
}

#3.Create coarse age datasets (0-64 and 65+) by cause; only need to run this once to get .rds output (saved in 1b_outcome_data)

for (subcauses in causes){
  
  print(subcauses)
  data <-  create_coarse_age_datasets(subcauses)
  
  for (subgroups in coarse_groups){
    
    print(subgroups)
    create_subgroup_datasets(subgroups)
  }
}
