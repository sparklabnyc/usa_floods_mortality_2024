rm(list=ls())

#0.Declare root directory, folder locations and load essential stuff
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'create_folder_structure.R'))
source(paste0(functions.folder,'script_initiate.R'))

#1.DO NOT RUN in this project:Process NCHS mortality data for each cause and year; raw mortality data is on RMP computer.
dat.all = data.frame()
for(year_current in years_analysis){
  
  # process deaths in given year 
  dat.temp = cause_process(year_current)
  dat.all = data.table::rbindlist(list(dat.all,dat.temp)) 
}

#write_csv(dat.all,
          #paste0(outcome.data.folder,'mortality_cs_fips_sex_age_',years_analysis[1],'_',years_analysis[length(years_analysis)],'.csv'))

#2.Tidy mortality data - this will run locally but is unnecessary 
mort_data <- read_csv(paste0(outcome.data.folder, "mortality_cs_fips_sex_age_2001_2018.csv")) #dataset from function in part 1 (above)

remove_non_specific_fips <- mort_data %>%   
  filter(!fips %in% c("08001", "08013", "08059", "08123", "12025", "30031", "30067","46113", "51560", "51515")) #remove FIPS that need to be recoded

recode_fips <- mort_data %>% 
  filter(fips %in% c("08001", "08013", "08059", "08123", "12025", "30031", "30067","46113", "51560", "51515")) %>% 
  mutate(fips = case_when(
    fips %in% c("08001", "08013", "08059", "08123") ~ "08014",
    fips %in% c("12025") ~ "12086",
    fips %in% c("30031", "30067") ~ "30113",
    fips %in% c("46113") ~ "46102",
    fips %in% c("51560") ~ "51005",
    fips %in% c("51515") ~ "51019",
    TRUE ~ fips
  ))
sum(remove_non_specific_fips$deaths)
sum(recode_fips$deaths)

mort_data <- bind_rows(remove_non_specific_fips, recode_fips)
rm(recode_fips, remove_non_specific_fips)

mort_data <- mort_data %>% 
  filter(!fips %in% c("05000", "38000", "53000")) #remove deaths in non-specific FIPS; includes only 3 deaths
sum(mort_data$deaths)

mort_data %>% write_csv(paste0(outcome.data.folder, "tidy_mortality_data.csv"))


