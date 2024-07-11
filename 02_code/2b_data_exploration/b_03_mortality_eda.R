rm(list = ls())

#0a.Declare root directory, folder location and load essential stuff
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'create_folder_structure.R'))
source(paste0(functions.folder,'script_initiate.R'))

#1a.Load data
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

#2.Mortality by season, sex, and age group 
basic_season_data <- health_outcome_flood_only %>% 
  rename("cause" = "group") %>% 
  dplyr::group_by(month, age, sex, cause) %>% 
  summarise(deaths = sum(deaths)) %>% 
  mutate(age_group = ifelse(age %in% c(0), 0,
                            ifelse(age %in% c(5,10),5,
                                   ifelse(age %in% c(15,20), 15,
                                          ifelse(age %in% c(25,30), 25,
                                                 ifelse(age %in% c(35,40), 35,
                                                        ifelse(age %in% c(45,50), 45,
                                                               ifelse(age %in% c(55, 60), 55,
                                                                      ifelse(age %in% c(65,70), 65,
                                                                             ifelse(age %in% c(75,80), 75,
                                                                                    ifelse(age %in% c(85), 85, 7))))))))))) %>% 
  group_by(month, age_group, sex, cause) %>% summarise(deaths = sum(deaths)) %>% 
  mutate(mo_days = ifelse(month %in% c(2), 28,
                          ifelse(month %in% c(1,3,5,7,8,10,12), 31,
                                 ifelse(month %in% c(4,6,9,11),30,0)))) %>% 
  mutate(stnd_deaths = round(31/mo_days*deaths), 0) %>% 
  mutate(sex = ifelse(sex %in% c(2), 0, 1))
basic_season_data$age_group <- as.factor(basic_season_data$age_group)
basic_season_data$sex <- factor(basic_season_data$sex, levels = c("0","1"), labels = c("Female", "Male"))
basic_season_data$age_group <- factor(basic_season_data$age_group, levels = c(0,5,15,25,35,45,55,65,75,85), 
                                      labels = c("0-4", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", "85+"))
basic_season_data$month <- factor(basic_season_data$month, levels = c(1,2,3,4,5,6,7,8,9,10,11,12),
                                  labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
basic_season_data$cause <- factor(basic_season_data$cause, levels = c("Injuries", "Cardiovascular diseases","Respiratory diseases",
                                                                      "Cancers", "Infectious and parasitic diseases", "Neuropsychiatric conditions"))

basic_season_data %>% write_csv(paste0(output.folder, "mort_data_by_season.csv"))

basic_season_data <- 
  read_csv(paste0(output.folder, "mort_data_by_season.csv"))
basic_season_data$month <- factor(basic_season_data$month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                                  labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

age.colors = c('grey','cornsilk', met.brewer(name = 'Isfahan1'))


options(scipen = 999)
seasonal_mortality_plot <- basic_season_data %>% 
  filter(cause != "Other") %>% 
  ggplot(aes(x=month, y = stnd_deaths,fill = age_group)) +
  geom_bar(stat = 'identity', position = "stack") + 
  scale_fill_manual(values = age.colors) +
  scale_y_continuous(labels = scales::comma) +
  facet_grid(sex~cause, labeller = labeller(cause = label_wrap_gen(width = 20))) +
  theme_bw() +
  ylab('Total deaths') +
  xlab ('Month') +
  guides(fill = guide_legend(title = "Age group (years)")) +
  theme(text = element_text(size = 12),
        legend.text=element_text(size=12), legend.title = element_text(size = 12),
        panel.grid.major = element_blank(),
        axis.text.x = element_text(size = 12, angle= 90), 
        axis.text.y = element_text(size=12),
        plot.margin=grid::unit(c(1,1,1,1), "mm"), 
        plot.title = element_text(hjust = 0.5), panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="white", size=.5, linetype="dotted"))

jpeg(paste0(figures.folder, 'Figure 1.jpeg'), res = 300, height = 2000, width = 3800)
seasonal_mortality_plot
dev.off()


#3.Mortality by season, sex, and year time series 
ts_data <- health_outcome_flood_only %>% 
  rename("cause" = "group") %>% 
  group_by(year, month, cause, age, sex) %>% 
  summarise(deaths = sum(deaths, na.rm = T), pop = sum(pop, na.rm = T)) %>% 
  left_join(pop_weights) %>% 
  mutate(asdr = (deaths/pop)*100000*weight) %>% 
  group_by(year, month, cause, sex) %>% 
  summarise(asdr = sum(asdr)) %>% 
  mutate(mo_days = ifelse(month %in% c(2), 28,
                          ifelse(month %in% c(1,3,5,7,8,10,12), 31,
                                 ifelse(month %in% c(4,6,9,11),30,0)))) %>% 
  mutate(stnd_deaths = (31/mo_days*asdr)) %>% 
  mutate(sex = ifelse(sex %in% c(2), 0, 1))

ts_data$new_mo <- factor(ts_data$month, levels = c(1,2,3,4,5,6,7,8,9,10,11,12),
                                  labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

ts_data$sex <- factor(ts_data$sex, levels = c("0","1"), labels = c("Female", "Male"))

ts_data %>% write_csv(paste0(output.folder, "mort_data_over_time.csv"))

options(scipen = 999)
ts_plot <- ts_data %>% 
  filter(cause != "Other") %>% 
  ggplot(aes(x=new_mo, y = stnd_deaths, group = as.factor(year), color = year)) +
  geom_line() + 
  facet_grid(sex~cause,labeller = labeller(cause = label_wrap_gen(width = 25))) + #scales = "free"
  ylab("Age-standardized death rate (per 100,000)") +
  xlab('Month') +
  theme_bw() +
  scale_color_gradientn(colours = c("darkgrey", "#D55E00"), name = "Year", breaks = c(2003, 2008, 2013,2018)) +
  guides(color = guide_colorbar(barwidth = 20)) +
  theme(legend.position = "bottom",
                axis.text.x = element_text(angle = 90),
        panel.grid = element_blank()) 

jpeg(paste0(figures.folder, 'Figure S2.jpeg'), res = 300, height = 2000, width = 3200)
ts_plot
dev.off()

#4.Descriptive table info 
table1_info <- health_outcome_flood_only %>% 
  group_by(sex, age, group) %>% 
  summarise(total_deaths = sum(deaths)) 
table1_info %>% write_csv(paste0(eda.output.folder, "table1_info.csv")) #Full table with counts by age, sex 


