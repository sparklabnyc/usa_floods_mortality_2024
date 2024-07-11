rm(list=ls())

#0.Load Packages
library(here)

#1a.Declare directories (can add to over time)
project.folder <-  paste0(print(here::here()),'/')

data.folder <-  paste0(project.folder, "01_data/")
  exposure.data.folder <- paste0(data.folder, '1a_exposure_data/')
  outcome.data.folder <- paste0(data.folder, '1b_outcome_data/')
  support.data.folder <- paste0(data.folder, '1c_supportive_datasets/')

code.folder <-  paste0(project.folder, "02_code/")
  data.prep.code.folder <-  paste0(code.folder, "2a_data_prep/")
  data.exploration.folder <-  paste0(code.folder, "2b_data_exploration/")
  models.folder <-  paste0(code.folder, "2c_models/")
  model.plotting.folder <-  paste0(code.folder, "2d_model_plotting/")
  functions.folder <- paste0(code.folder, "20_functions/")


output.folder <-  paste0(project.folder, "03_output/")
  eda.output.folder <- paste0(output.folder, "3a_eda_output/")
  model.output.folder <- paste0(output.folder, "3b_model_output/")

tables.folder <- paste0(project.folder, "04_tables/")
figures.folder <- paste0(project.folder, "05_figures/")
lit.folder <- paste0(project.folder, "06_literature/")
drafts.folder <- paste0(project.folder, "07_drafts/")


#1b.Identify list of folder locations which have just been created above
folders.names <-  grep(".folder",names(.GlobalEnv),value=TRUE)

#1c.Create function to create list of folders
# note that the function will not create a folder if it already exists 
create_folders <-  function(name){
  ifelse(!dir.exists(get(name)), dir.create(get(name), recursive=TRUE), FALSE)
}

#1d.Create the folders named above
lapply(folders.names, create_folders)