rm(list = ls())

#0.Declare root directory, folder location and load essential stuff
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'create_folder_structure.R'))
source(paste0(functions.folder,'script_initiate.R'))
#census_api_key("9a6713f3a30baf963e6cde34d7dac806df34a151", overwrite = TRUE)

#1.DO NOT RUN in this project: Load and join year-specific monthly population data from Robbie Parks' CDC
#Monthly Population Inference project. File structure and data are from his project. 
dat_all = data.frame()
for(year_selected in years_analysis){
  dat_year <- read_csv(paste0(population.5year.processed.folder, 'vintage_2020/pop_monthly_5_year_age_groups_',year_selected,'.csv'))
  dat_all <- data.table::rbindlist(list(dat_all, dat_year))
  rm(dat_year)
}

head(dat_all)
unique(dat_all$year)

#The output from this is cdc_population_monthly_infer.csv saved in 1c_supportive datasets.  