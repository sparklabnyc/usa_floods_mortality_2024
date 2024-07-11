rm(list = ls())

#0a.Declare root directory, folder location and load essential stuff
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'create_folder_structure.R'))
source(paste0(functions.folder,'script_initiate.R'))

#0b.Load flood datasets
gfd_with_flood_type <- read_csv(paste0(exposure.data.folder, "gfd_with_flood_type.csv")) %>% 
  janitor::clean_names() %>% 
  filter(!state %in% c("AK", "PR", "VI"))

flood_data_with_pop <- read_csv(paste0(exposure.data.folder, "flood_pop_data_by_thresh_type.csv")) %>% 
  mutate(fips = geoid)
n_distinct(flood_data_with_pop$dfo_id)

#1.County-flood count by exposure threshold and flood cause 
flood_county_table <- flood_data_with_pop %>% 
  filter(flood_occur == 1) %>% 
  dplyr::select(geoid,dfo_began,expo_threshold, flood_cat) %>% distinct() %>%
  na.omit() %>% 
  group_by(expo_threshold, flood_cat) %>% tally() %>% 
  spread(flood_cat, n)
flood_county_table$expo_threshold <- factor(flood_county_table$expo_threshold, levels = c("any", "1_pert","25_pert","50_pert","75_pert"),
                                            labels = c("Any", "<25th percentile","25th percentile", "50th percentile", "75th percentile"))
  flood_county_table %>% write_csv(paste0(tables.folder, "Table S2_flood_spf.csv"))

#2.Flood count and percent of total by flood cause 
count_by_flood_type <- flood_data_with_pop %>%
  dplyr::select(dfo_id, flood_cat) %>% distinct() %>% 
  group_by(flood_cat) %>% tally() %>% 
  mutate(perc_floods = n/sum(n))
  count_by_flood_type %>% write_csv(paste0(tables.folder, "Table S5.csv"))


#3a.Count of floods by county (geoid)
dat_summary_flood <-  gfd_with_flood_type %>% 
  filter(flood_pix_sum>0 & ghsl_popexp2015 > 0) %>%
  dplyr::select(geoid, state, dfo_began, dfo_id) %>% distinct() |> 
  group_by(geoid, state) %>%
  tally() %>%
  replace(is.na(.), 0)

#3b.Histograms of floods per county by state 
dat_summary_flood %>% ggplot(aes(n)) +
  geom_histogram(bins = 20) +
  xlab("Floods per county (any exposure by pixel)") +
  ylab('Count') +
  facet_wrap(~state, scales = "free_y") + #scales = 'free'
  theme_bw()

#3c.Histogram of duration
gfd_with_flood_type %>% 
  dplyr::select(geoid, state, dfo_id, flood_days_mean, flood_days_max,flood_cat) %>% 
  filter(flood_days_mean > 0) %>% 
  ggplot(aes(flood_days_max)) +
  geom_histogram(bins = 100) +
  facet_wrap(~flood_cat, scales = "free_y") + #scales = 'free'
  xlab("Median avg. flood duration (days) by county") +
  ylab('Count') +
  theme_bw()  

#4.Table of duration by flood type and severity 
flood_duration <- flood_data_with_pop %>%   
  filter(flood_occur == 1 & expo_type == "pop_expo") %>% distinct() %>% 
  dplyr::select(fips,flood_days_max,flood_days_mean,state,year,flood_cat,expo_threshold) %>% 
  group_by(flood_cat, expo_threshold) %>% 
  summarize(mean_dur = mean(flood_days_mean), 
            med_dur = median(flood_days_mean),
            max_mean_dur = mean(flood_days_max),
            max_med_dur = median(flood_days_max))
flood_duration$expo_threshold <- factor(flood_duration$expo_threshold, levels = c("any","1_pert","25_pert","50_pert","75_pert"),
                                             labels = c("Any", "< 25th percentile","25th percentile", "50th percentile", "75th percentile"))

flood_duration %>% dplyr::select(flood_cat, expo_threshold,med_dur) %>% spread(expo_threshold, med_dur) %>%
  write_csv(paste0(tables.folder, "Tables S6a.csv"))

flood_duration %>% dplyr::select(flood_cat, expo_threshold,max_med_dur) %>% spread(expo_threshold, max_med_dur) %>%
  write_csv(paste0(tables.folder, "Tables S6b.csv"))

#5.Assess differences in flood duration by flood type
pairwise.wilcox.test(flood_duration$med_dur, flood_duration$flood_cat,
                     p.adjust.method = "BH")

ggplot(flood_duration, aes(x = flood_cat, y = med_dur)) +
  geom_boxplot(color = "red", fill = "orange", alpha = 0.2) +
  facet_wrap(~expo_threshold)

#6.Table of average number of exposed counties by flood cause and severity 
flood_expo_counties <- flood_data_with_pop %>% 
  filter(flood_occur == 1) %>% 
  dplyr::select(geoid, dfo_began, dfo_id, flood_cat, expo_threshold) %>% distinct() %>%
  group_by(dfo_began, dfo_id,flood_cat, expo_threshold) %>%
  tally() %>%
  group_by(flood_cat, expo_threshold) %>%
  mutate(mean_num_expo_counties = mean(n),
         median_num_expo_counties = median(n)) |> 
  dplyr::select(flood_cat, expo_threshold, mean_num_expo_counties, median_num_expo_counties) %>% distinct() %>%
  na.omit()

flood_expo_counties %>% dplyr::select(flood_cat, expo_threshold, median_num_expo_counties) %>% 
  spread(expo_threshold, median_num_expo_counties) %>% write_csv(paste0(tables.folder,"Tables SXa.csv"))

flood_expo_counties %>% dplyr::select(flood_cat, expo_threshold, mean_num_expo_counties) %>% 
  spread(expo_threshold, mean_num_expo_counties) %>% write_csv(paste0(tables.folder,"Tables SXb.csv"))


#7.Time series of flood events by flood type
flood_seasonality <- read_csv(paste0(exposure.data.folder, "flood_pop_data_by_thresh_type.csv")) %>% 
  dplyr::select(dfo_began, month, year, flood_cat) %>% distinct() %>% 
  group_by(month, flood_cat) %>% tally()

flood_seasonality$flood_cat <- factor(flood_seasonality$flood_cat, levels = c("Ice jam or dam break", "Snowmelt", "Tropical cyclone", "Heavy rain"))
flood_seasonality$month <- factor(flood_seasonality$month, levels = c(1,2,3,4,5,6,7,8,9,10,11,12),
                                  labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
flood.type.colors = c("#e3c28b", "lightblue", "#178f92", "darkblue")


seasonal_floods_plot <- flood_seasonality %>% 
  ggplot(aes(x=month, y = n,fill = flood_cat)) +
  geom_bar(stat = 'identity', position = "stack") + 
  scale_fill_manual(values = flood.type.colors) + 
  scale_y_continuous(labels = scales::comma, limits = c(0,15)) +
  theme_bw() +
  ylab('Flood count') +
  xlab ('Month') +
  guides(fill = guide_legend(title = "Flood category")) +
  theme(legend.position = "bottom",
        #axis.text.x = element_text(angle = 90),
        panel.grid = element_blank()) 

jpeg(paste0(figures.folder, 'Figure S3.jpeg'), res = 300, height = 2000, width = 3000)
seasonal_floods_plot
dev.off()

