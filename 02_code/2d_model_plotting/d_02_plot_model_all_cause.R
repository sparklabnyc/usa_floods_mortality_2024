rm(list = ls())
#0a.Declare root directory, folder location and load essential stuff
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'create_folder_structure.R'))
source(paste0(functions.folder,'script_initiate.R'))

#1.Load datasets
overall_results <- read_csv(paste0(model.output.folder,"model_results_all_mort_causes.csv"))

#2.Load and prepare model output data
plot_data <- overall_results %>% 
  filter(str_detect(rowname, "lag_")) %>% 
  mutate(across(all_of("rowname"), str_remove,pattern = "\\...*")) %>% 
  mutate(rowname = case_when(rowname == 'lag_0' ~ '0', 
                             rowname == 'lag_1' ~ '1', 
                             rowname == 'lag_2' ~ '2', 
                             rowname == 'lag_3' ~ '3')) %>% 
  mutate(flood_cat = case_when(flood_cat == "Snowmelt" ~ "Snowmelt",
                               flood_cat == "Heavy rain" ~ "Heavy rain",
                               flood_cat == "Tropical cyclones" ~ "Tropical cyclone",
                               flood_cat == "Ice jams and dam breaks" ~ "Ice jam or dam break",
                               flood_cat == "all_floods" ~ "All floods")) |> 
  rename(lower_ci = '0.025quant',
         upper_ci = '0.975quant') %>% 
  mutate(threshold = case_when(threshold == '1_pert' ~ 'a1_pert', 
                               threshold == '25_pert' ~ 'b25_pert', 
                               threshold == '50_pert' ~ 'c50_pert', 
                               threshold == '75_pert' ~ 'd75_pert'))

#2.Set labels, colors, etc. for figures
causes.labs <- c("Injuries", "Cardiovascular diseases","Respiratory diseases","Cancers", "Infectious and\nparasitic diseases","Neuropsychiatric\nconditions" )
floodtypes.labs <- c("Heavy rain", "Snowmelt", "Tropical cyclone", "Ice jam or dam break")
names(causes.labs) <- c("Injuries", "Cardiovascular diseases","Respiratory diseases","Cancers", "Infectious and parasitic diseases", "Neuropsychiatric conditions")
names(floodtypes.labs) <- c("Heavy rain", "Snowmelt", "Tropical cyclone", "Ice jam or dam break")

#3a.Set plots to run
plot_groups <- c("overall")
expo_types <- c("pop_expo")
expo_thresholds <- c("1_pert", "25_pert", "50_pert", "75_pert")

#3b.Make all severity plots
plot_overall <-  ggplot(data=plot_data) +
    geom_errorbar(data = filter(plot_data, threshold == "a1_pert"),
                  aes(x=rowname, ymax=upper_ci,ymin=lower_ci, color = threshold), size = 0.5, width = 0, color = "black",
                  position = position_nudge(x = -0.225)) +
    geom_point(data = filter(plot_data, threshold == "a1_pert"), 
               aes(x=rowname,y=mean, color = threshold), size = 3, position = position_nudge(x = -0.225)) +
    
    
    geom_errorbar(data = filter(plot_data, threshold == "b25_pert"),
                  aes(x=rowname, ymax=upper_ci,ymin=lower_ci, color = threshold), size = 0.5, width = 0, color = "black",
                  position = position_nudge(x = -0.075)) +
    geom_point(data = filter(plot_data, threshold == "b25_pert"), 
               aes(x=rowname,y=mean, color = threshold), size = 3, position = position_nudge(x = -0.075)) +
    
    
    geom_errorbar(data = filter(plot_data, threshold == "c50_pert"),
                  aes(x=rowname, ymax=upper_ci,ymin=lower_ci, color = threshold), size = 0.5, width = 0, color = "black",
                  position = position_nudge(x = 0.075)) +
    geom_point(data = filter(plot_data, threshold == "c50_pert"), 
               aes(x=rowname,y=mean, color = threshold), size = 3, position = position_nudge(x = 0.075)) +
    
    geom_errorbar(data = filter(plot_data, threshold == "d75_pert"),
                  aes(x=rowname, ymax=upper_ci,ymin=lower_ci, color = threshold), size = 0.5, width = 0, color = "black",
                  position = position_nudge(x = 0.225)) +
    geom_point(data = filter(plot_data, threshold == "d75_pert"), 
               aes(x=rowname,y=mean, color = threshold), size = 3, position = position_nudge(x = 0.225)) +
    
    geom_hline(yintercept=0,linetype='dotted') +
    xlab('Month after flood event') + ylab('Percent change in death rates') +
    scale_color_manual(values=c("#4EB3D3","#2B8CBE", "#0868AC", "#084081"), guide = guide_legend(nrow = 1,title = paste0("Flood severity")),
                       labels = c("Mild","Moderate","Severe", "Very severe")) +
    scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
    scale_shape_manual(values=seq(0,15)) +
    labs(colour="", shape="") +
    theme_bw() + theme(text = element_text(size = 12),
                       legend.text=element_text(size=12), legend.title = element_text(size = 12),
                       panel.grid.major = element_blank(),axis.text.x = element_text(size = 12, angle= 0), axis.text.y = element_text(size=10),
                       plot.margin=grid::unit(c(0.5,0.5,0.5,0.5), "mm"), 
                       plot.title = element_text(hjust = 0.5), panel.background = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                       panel.border = element_rect(colour = "black"),strip.background = element_blank(),
                       legend.position = 'bottom',legend.justification='center',
                       legend.background = element_rect(fill="white", size=.5, linetype="dotted")) +
    #facet_wrap(~flood_cat, scales = "free_y") +
    facet_wrap(~flood_cat) +
    theme(panel.spacing = unit(0.6, "cm"))
  
  jpeg(paste0(figures.folder,'all_cause_mort.jpeg'), res = 400, height = 3000, width = 3500)
  print(plot_overall)
  dev.off()



