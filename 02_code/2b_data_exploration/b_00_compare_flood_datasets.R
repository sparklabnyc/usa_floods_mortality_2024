rm(list = ls())

#0a.Declare root directory, folder location and load essential stuff
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'create_folder_structure.R'))
source(paste0(functions.folder,'script_initiate.R'))
#census_api_key("9a6713f3a30baf963e6cde34d7dac806df34a151", overwrite = TRUE)

#This code is used to examine floods that are not included in the GFD but are present in the DFO; it does not yield results
#used in the analysis. 

#0b.Load datasets
add_flood_type <- read_csv(paste0(exposure.data.folder, "gfd_with_flood_type.csv"))
flood_archive <- read_csv(paste0(exposure.data.folder, "FloodArchive.csv")) %>% rename(dfo_id = "ID")
floods_not_in_gfd <- read_csv(paste0(exposure.data.folder, "floods_not_in_gfd.csv")) %>%
  rename(category = 'category.x',
         dfo_id = 'ID')

#1.Flood events from GFD by coordinates 
#1a.Map flood events by flood category
floods_gfd_map <- left_join(add_flood_type, flood_archive, by = c("dfo_id", "MainCause", "category", "Validation")) %>% 
  dplyr::select(dfo_id, dfo_lat, dfo_lon, category) %>% distinct()
map_floods_gfd <- st_as_sf(floods_gfd_map, coords = c("dfo_lon", "dfo_lat"), crs = 4326)
mapview(map_floods_gfd, map.types = "Stamen.Toner", zcol = "category")

#1b.Count of floods by category
gfd_flood_count <- floods_gfd_map %>% dplyr::select(dfo_id, category) %>% distinct() %>% 
  group_by(category) %>% tally() %>% 
  mutate(percent_all_floods = n/sum(n)*100)

#2.Flood events in DFO but not in GFD by coordinates
#2a.Map DFO only flood events
map_floods_dfo_only <- st_as_sf(floods_not_in_gfd, coords = c("long", "lat"), crs = 4326)
mapview(map_floods_dfo_only, map.types = "Stamen.Toner", zcol = "category")

#2b.Count of floods by category 
dfo_flood_count <- floods_not_in_gfd %>% dplyr::select(dfo_id, category) %>% distinct() %>% 
  group_by(category) %>% tally() %>% 
  mutate(percent_all_floos = n/sum(n)*100)

#3.Map comparing DFO and GFD flood events
compare_map <- floods_not_in_gfd %>% dplyr::select(dfo_id, long,lat, category) %>% distinct() %>% 
  rename(dfo_lon = long, dfo_lat = lat) %>% 
  mutate(dfo = 1) %>% 
  bind_rows(floods_gfd_map) %>% 
  mutate(dfo = replace_na(dfo, 0))
map_dfo_gfd <- st_as_sf(compare_map, coords = c("dfo_lon", "dfo_lat"), crs = 4326)
mapview(map_dfo_gfd, map.types = "Stamen.Toner", zcol = "dfo")


#4.Identify floods in NCEI (NOAA storm database) but not in GFD and/or DFO
floods_ncei <- read_csv(paste0(exposure.data.folder, "ncei_usa_county_panel_20230222.csv")) %>% 
  filter(event_type %in% c("Flood", "Flash Flood", "Heavy Rain", "Storm Surge/Tide", "Hurricane (Typhoon)", "Tropical Storm",
                           "Coastal Flood", "Lakeshore Flood", "Tropical Depression", "Hurricane")) %>% 
  mutate(begin_date = as_date(begin_date, formate = '%Y-%m-%d'),
         began = ymd(begin_date), 
         year = year(began),
         month = month(began)) %>% 
  filter(between(year, 2001, 2018)) %>% 
  filter(!state %in% c("American Samoa", "Guam", "Virgin Islands", "Puerto Rico"))

#4a.Restrict dataset to just cyclonic storms 
ncei_storms <- floods_ncei %>% filter(event_type %in% c("Hurricane (Typhoon)", "Hurricane", "Tropical Storm")) %>% 
  dplyr::select(begin_date, state, event_type) %>% distinct()

