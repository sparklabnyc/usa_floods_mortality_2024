rm(list = ls())

#0a.Declare root directory, folder location and load essential stuff
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'create_folder_structure.R'))
source(paste0(functions.folder,'script_initiate.R'))
source(paste0(data.folder, 'map_objects.R'))

#0b.Load flood datasets
flood_data_map <- read_csv(paste0(exposure.data.folder, "flood_pop_data_by_thresh_type.csv"))
states <- us_map("states") %>% filter(!abbr %in% c("AK", "HI"))

#1.Prepare data for mapping

#1a.Tidy flood data
map_flood_events <- flood_data_map %>% 
  filter(quant_pop_flood > 0 & flood_occur == 1) %>% 
  filter(expo_threshold == "any") %>% distinct() %>% 
  add_count(geoid,flood_cat) %>% 
  rename(count = nn) %>% 
  group_by(geoid,flood_cat,count) %>% 
  summarize(mean_avg_dur = mean(flood_days_mean),
            median_avg_dur = median(flood_days_mean),
            mean_max_dur = mean(flood_days_max),
            median_max_dur = median(flood_days_max)) %>% 
  mutate(across(c('median_avg_dur', 'median_max_dur'), round, 0)) |> 
  pivot_longer(cols = 3:7,
               names_to = "map_var_type",
               values_to = "map_var_value")

#1b.Join flood events to map data
USA.df.summary.flood <-  left_join(USA.df,map_flood_events,by=c('GEOID' = 'geoid')) %>%
  replace(is.na(.), 0) %>% 
  filter(!STATEFP %in% c(02,15))
colMeans(is.na(USA.df.summary.flood)) 

#1c.Create settings table for iteration 
map_variables <- c("count", "median_avg_dur", "median_max_dur")
flood_cats <- c("Heavy rain", "Snowmelt", "Tropical cyclone", "Ice jam or dam break")

map_settings <- expand_grid(flood_cats, map_variables) |> 
  mutate(color = case_when(
    (flood_cats == "Heavy rain" & map_variables == "count") ~ 'heavy.rain.floods.map', 
    (flood_cats == "Snowmelt" & map_variables == "count") ~ 'rain.snow.floods.map', 
    (flood_cats == "Tropical cyclone" & map_variables == "count") ~ 'storms.floods.map', 
    (flood_cats == "Ice jam or dam break" & map_variables == "count") ~ 'ice.jam.floods.map', 
    map_variables == 'median_avg_dur' ~ 'all.floods.map',
    map_variables == 'median_max_dur' ~ 'all.floods.map'
  )) |> 
  mutate(label = case_when(
    map_variables == "count" ~ ' count',
    map_variables == "median_avg_dur" ~ 'median duration',
    map_variables == "median_max_dur" ~ 'median maximum duration'
  ))

#2.Map: counts of flood events 
#2a.Count maps
rain <- map_floods_function("count", "Heavy rain")
tc <- map_floods_function("count", "Tropical cyclone")
snow <- map_floods_function("count", "Snowmelt")
ice <- map_floods_function("count", "Ice jam or dam break")

#2b.Save count 2x2 plots 
jpeg(paste0(figures.folder, 'Figure 2.jpeg'), res = 300, height = 2000, width = 3000)
ggarrange(rain, tc, snow, ice,
          ncol = 2, nrow = 2,
          legend = "bottom", 
          labels = c("a)", "b)", "c)", "d)"),
          common.legend = FALSE)
dev.off()

#3.Map: median duration of flood events
#3a.Make maps
rain <- map_floods_function("median_avg_dur", "Heavy rain")
tc <- map_floods_function("median_avg_dur", "Tropical cyclone")
snow <- map_floods_function("median_avg_dur", "Snowmelt")
ice <- map_floods_function("median_avg_dur", "Ice jam or dam break")

#3b.Save median duration 2x2 plots 
jpeg(paste0(figures.folder, 'Figure S4.jpeg'), res = 300, height = 2000, width = 3000)
ggarrange(rain, tc, snow, ice,
          ncol = 2, nrow = 2,
          legend = "bottom", 
          labels = c("a)", "b)", "c)", "d)"),
          common.legend = FALSE)
dev.off()

#4.Map: median max duration of flood events
#4a.Make maps
rain <- map_floods_function("median_max_dur", "Heavy rain")
tc <- map_floods_function("median_max_dur", "Tropical cyclone")
snow <- map_floods_function("median_max_dur", "Snowmelt")
ice <- map_floods_function("median_max_dur", "Ice jam or dam break")

#4b.Save median max duration 2x2 plots 
jpeg(paste0(figures.folder, 'Figure S5.jpeg'), res = 300, height = 2000, width = 3000)
ggarrange(rain, tc, snow, ice,
          ncol = 2, nrow = 2,
          legend = "bottom", 
          labels = c("a)", "b)", "c)", "d)"),
          common.legend = FALSE)
dev.off()



