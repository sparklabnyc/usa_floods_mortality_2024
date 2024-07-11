rm(list = ls())

#0a.Declare root directory, folder location and load essential stuff
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'create_folder_structure.R'))
source(paste0(functions.folder,'script_initiate.R'))

#Numbers for mortality 
basic_season_data <- read_csv(paste0(output.folder, "mort_data_by_season.csv")) #this includes just fips with a flood excluding HI, AK, PR, VI

#Total number of deaths by mortality cause
total_deaths_by_cause <- basic_season_data %>% 
  na.omit() %>% 
  group_by(cause) %>%
  summarize(deaths = sum(deaths)) %>%
  mutate(total_deaths = sum(deaths),
         perc_deaths = deaths/total_deaths)

deaths_in_incl_flood_fips <- total_deaths_by_cause$total_deaths[1]
total_deaths_by_cause %>% write_csv(paste0(tables.folder, "Table S4.csv")) #Summary table with counts just by mortality cause 

#Total number of deaths by sex
deaths_by_sex <- basic_season_data %>% 
  na.omit() |> 
  filter(!is.na(cause)) %>% 
  group_by(sex) %>% 
  summarize(total_deaths = sum(deaths,na.rm=TRUE))
sum(deaths_by_sex$total_deaths,na.rm = TRUE)

#Total number of deaths by age category
deaths_by_age <- basic_season_data %>% 
  filter(!is.na(cause)) %>% 
  mutate(age_cat = case_when(age_group %in% c("65-74", "75-84", "85+") ~ '1')) %>% 
  mutate(age_cat = replace_na(age_cat,'0')) %>% 
  group_by(age_cat) %>% 
  summarize(total_deaths = sum(deaths,na.rm=TRUE))
sum(deaths_by_age$total_deaths)

#To find a specific death count by cause or demographic variable
basic_season_data %>% 
  filter(cause == "Injuries" ) %>% 
  mutate(all_deaths = sum(deaths)) %>% 
  group_by(age_group, sex,all_deaths) %>% summarize(deaths_age = sum(deaths)) %>% 
  mutate(pert_deaths = deaths_age/all_deaths) %>% 
  filter(!age_group %in% c("55-64", "65-74", "75-84", "85+")) %>% ungroup() %>% 
  filter(sex == "Male") %>% 
  mutate(prop_under_55 = sum(pert_deaths))

ts_data <- read_csv(paste0(output.folder, "mort_data_over_time.csv"))

ts_data %>%
  #filter(sex == "Female") %>% 
  group_by(year,cause) %>% 
  summarize(deaths = sum(asdr)) %>% na.omit() %>% 
  filter(year %in% c(2001,2018)) %>% 
  group_by(cause) %>% 
  mutate(change = diff(deaths[year %in% 2001:2018])) %>% 
  filter(year == 2001) %>% 
  mutate(cot = change/deaths*100)

#Summary numbers for floods
flood_data <- read_csv(paste0(exposure.data.folder, "flood_pop_data_by_thresh_type.csv"))

flood_county_months <- flood_data %>% 
  dplyr::select(geoid, month, year, dfo_id) %>% distinct() %>% 
  group_by(geoid) %>% tally() %>% 
  mutate(total_num_flood_months = sum(n)) #number of flood events per county and total number of flood-county events (19,661)

n_distinct(flood_county_months$geoid) #number of counties that experience a flood (2,711)
mean(flood_county_months$n)
median(flood_county_months$n)
max(flood_county_months$n)

#Number of counties exposed per flood 
counties_exposed_per_flood <-flood_data %>% 
  dplyr::select(dfo_began, geoid, dfo_id) %>% distinct() %>% group_by(dfo_began, dfo_id) %>% tally()

#Calculate the total number of deaths in all fips and flooded fips

#1a.Load encessary data
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

#1b.Join mortality and population data
health_outcome_data <- left_join(mort_data, pop_data) 
health_outcome_flood_only <- left_join(flood_fips,health_outcome_data)

#1c.i) Calculate total number of deaths in all counties
all_deaths <- sum(health_outcome_data$deaths) #45,769,292
count_deaths_by_cause_all_fips <- health_outcome_data |> group_by(group) |> summarize(deaths = sum(deaths, na.rm = TRUE))
#6,342,038 in 'other' category
(all_deaths - 6342038) / all_deaths #86.1% 

#1c.ii) Calculate total number of deaths in flooded counties  
deaths_in_flood_fips <- sum(health_outcome_flood_only$deaths, na.rm = TRUE) #41,324,531
deaths_in_flood_fips/all_deaths #90.3% of deaths in US are in flooded FIPS

count_deaths_by_cause_flood_fips <- health_outcome_flood_only |> group_by(group) |> summarize(deaths = sum(deaths, na.rm = TRUE))
#5,711,133 in 'other' category
#35,613,398 in all other categories
(deaths_in_flood_fips - 5711133) / deaths_in_flood_fips #86.2% of deaths in flooded fips fall in 6 primary categories 

(deaths_in_flood_fips - 5711133)/all_deaths 
#77.8% of all deaths during study period occurred in flood counties among 6 main mortality cause groups
