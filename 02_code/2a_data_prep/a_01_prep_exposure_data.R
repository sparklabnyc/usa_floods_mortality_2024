rm(list = ls())

#0a.Declare root directory, folder location and load essential stuff
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'create_folder_structure.R'))
source(paste0(functions.folder,'script_initiate.R'))

#1.Load Global Flood Database (GFD) data


gfd_data <- read_csv(paste0(exposure.data.folder, "gfd_county_panel.csv")) %>% 
  mutate(dfo_began = mdy(dfo_began),
         dfo_ended = mdy(dfo_ended),
         year = year(dfo_began),
         month = month(dfo_began)) %>% 
  filter(!dfo_id %in% c(2380, 2198,2852,3298,3218,2717,3871,4197,3184,3199,3332)) #remove these flood events because they do not affect any county in US
n_distinct(gfd_data$dfo_id) #


#2.Load Dartmouth Flood Observatory (DFO) Flood Archive data
  #2a.identify flood events correctly labeled as occurring in US
  flood_events_part1 <- read_csv(paste0(exposure.data.folder, "FloodArchive.csv")) %>% 
    filter(Country %in% c("USA", "USA.", "Canada and USA")) %>% 
    dplyr::rename("dfo_id" = ID) %>% 
    mutate(Began = mdy(Began), 
         Ended = mdy(Ended),
         year = year(Began),
         month = month(Began)) %>% 
    dplyr::select(dfo_id, MainCause, category, Validation, year) %>% 
    filter(between(year, 2001, 2018))
  
  #2b.identify flood events incorrectly labeled as occurring outside of US (Mexico, Canada, Haiti, Jamaica) that actually affect US counties
  flood_events_part2 <- read_csv(paste0(exposure.data.folder, "FloodArchive.csv")) %>% 
    dplyr::rename("dfo_id" = ID) %>% 
    filter(dfo_id %in% c(1972, 2356,2566,2753,2841,3336,3655,3671,3673,3812,4695,3092, 2562, 4302,4524,4612)) %>% #flood events that should be included; problem is that most aren't in GFD 
    mutate(Began = mdy(Began), 
           Ended = mdy(Ended),
           year = year(Began),
           month = month(Began)) %>% 
    dplyr::select(dfo_id, MainCause, category, Validation, year)
  
  #2c.join all flood events
  flood_events <- bind_rows(flood_events_part1, flood_events_part2)

#3.Add DFO flood type to GFD 
add_flood_type <- left_join(gfd_data, flood_events) %>% 
mutate(flood_cat = case_when(is.na(category) ~ 'none',
                             category == "Heavy rain" ~ "Heavy rain",
                             category == "Brief torrential rain" ~ "Heavy rain",
                             category == "Heavy rain and snowmelt" ~ "Snowmelt",
                             category == "Snowmelt" ~ "Snowmelt",
                             category == "Tropical storms" ~ "Tropical cyclone",
                             category == "Hurricane" ~ "Tropical cyclone",
                             category == "Ice jams" ~ "Ice jam or dam break",
                             category == "Dam failure" ~ "Ice jam or dam break", 
))
  add_flood_type %>% 
    write_csv(paste0(exposure.data.folder, "gfd_with_flood_type.csv"))
  
#4.Identify floods in DFO dataset but NOT in GFD
floods_not_in_gfd <- anti_join(flood_events_part1, gfd_data) 
flood_archive <- read_csv(paste0(exposure.data.folder, "FloodArchive.csv")) %>% 
  rename(dfo_id = "ID")
floods_not_in_gfd <- left_join(floods_not_in_gfd, flood_archive, by = c("dfo_id", "MainCause", "category", "Validation")) 
  floods_not_in_gfd %>% 
    write_csv(paste0(exposure.data.folder, "floods_not_in_gfd.csv"))
