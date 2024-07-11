rm(list = ls())

#0a.Declare root directory, folder location and load essential stuff
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'create_folder_structure.R'))
source(paste0(functions.folder,'script_initiate.R'))

#0b.Load datasets
gfd_data <- read_csv(paste0(exposure.data.folder, "gfd_with_flood_type.csv")) 

#1.Calculate percent of population exposed for each county and each flood event 

#1a.Quantiles based on overall floods
pop_expo_overall <- gfd_data %>% 
  filter(ghsl_popexp2015 > 0.01) %>% 
  mutate(perc_pop_flood_2015 = ((ghsl_popexp2015/ghsl_pop2015)*100), 
         quant_pop_flood = ntile(perc_pop_flood_2015, 4)) 

#1b.Quantiles based on specific flood causes 
pop_expo_flood_type <- gfd_data %>% 
  filter(ghsl_popexp2015 > 0.01) %>% 
  group_by(flood_cat) |> 
  mutate(perc_pop_flood_2015 = ((ghsl_popexp2015/ghsl_pop2015)*100), 
         quant_pop_flood = ntile(perc_pop_flood_2015, 4)) 

#2.Histogram for overall flood exposure 
quants <- quantile(pop_expo_overall$perc_pop_flood_2015, probs = c(0.25,0.5,0.75))  

overall_hist <- pop_expo_overall %>% 
  #mutate(x_new = ifelse(perc_pop_flood_2015 > 0.005, 0.005, perc_pop_flood_2015)) %>%
  ggplot(aes(perc_pop_flood_2015)) + geom_histogram(binwidth = 0.0001, col = "black", fill = "grey") + 
  geom_vline(xintercept = quants[1], color = "green3", size = 1.0) + 
  geom_vline(xintercept = quants[2], color = "blue", size = 1.0) +
  geom_vline(xintercept = quants[3], color = "red", size = 1.0) + 
  theme_bw() + theme(text = element_text(size = 16),
                     legend.text=element_text(size=12), legend.title = element_text(size = 12),
                     panel.grid.major = element_blank(),axis.text.x = element_text(size = 12, angle= 0), axis.text.y = element_text(size=10),
                     plot.margin=grid::unit(c(0.5,0.5,0.5,0.5), "cm"), 
                     plot.title = element_text(hjust = 0.5), panel.background = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
                     legend.position = 'bottom',legend.justification='center',
                     legend.background = element_rect(fill="white", size=.5, linetype="dotted")) +
  xlab('') + ylab('') +
  scale_y_continuous(expand = c(0,0), limits = c(0,500)) + 
  scale_x_continuous(limits = c(0,0.2),expand = c(0,0))

#3.Histogram for heavy rain
rain <- pop_expo_flood_type |> 
  filter(flood_cat == "Heavy rain")

quants <- quantile(rain$perc_pop_flood_2015, probs = c(0.25,0.5,0.75))  

rain_hist <- rain %>% 
  ggplot(aes(perc_pop_flood_2015)) + geom_histogram(binwidth = 0.0001, col = "black", fill = "grey") + 
  geom_vline(xintercept = quants[1], color = "green3", size = 1.0) + 
  geom_vline(xintercept = quants[2], color = "blue", size = 1.0) +
  geom_vline(xintercept = quants[3], color = "red", size = 1.0) + 
  theme_bw() + theme(text = element_text(size = 16),
                     legend.text=element_text(size=12), legend.title = element_text(size = 12),
                     panel.grid.major = element_blank(),axis.text.x = element_text(size = 12, angle= 0), axis.text.y = element_text(size=10),
                     plot.margin=grid::unit(c(0.5,0.5,0.5,0.5), "cm"), 
                     plot.title = element_text(hjust = 0.5), panel.background = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
                     legend.position = 'bottom',legend.justification='center',
                     legend.background = element_rect(fill="white", size=.5, linetype="dotted")) +
  xlab('') + ylab('') +
  scale_y_continuous(expand = c(0,0), limits = c(0,350)) + 
  scale_x_continuous(limits = c(0,0.2),expand = c(0,0))

#4.Histogram for tropical cyclones
tcs <- pop_expo_flood_type |> 
  filter(flood_cat == "Tropical cyclone")

quants <- quantile(tcs$perc_pop_flood_2015, probs = c(0.25,0.5,0.75))  

tcs_hist <- tcs %>% 
  ggplot(aes(perc_pop_flood_2015)) + geom_histogram(binwidth = 0.0001, col = "black", fill = "grey") + 
  geom_vline(xintercept = quants[1], color = "green3", size = 1.0) + 
  geom_vline(xintercept = quants[2], color = "blue", size = 1.0) +
  geom_vline(xintercept = quants[3], color = "red", size = 1.0) + 
  theme_bw() + theme(text = element_text(size = 16),
                     legend.text=element_text(size=12), legend.title = element_text(size = 12),
                     panel.grid.major = element_blank(),axis.text.x = element_text(size = 12, angle= 0), axis.text.y = element_text(size=10),
                     plot.margin=grid::unit(c(0.5,0.5,0.5,0.5), "cm"), 
                     plot.title = element_text(hjust = 0.5), panel.background = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
                     legend.position = 'bottom',legend.justification='center',
                     legend.background = element_rect(fill="white", size=.5, linetype="dotted")) +
  xlab('') + ylab('') +
  scale_y_continuous(expand = c(0,0), limits = c(0,100)) + 
  scale_x_continuous(limits = c(0,0.2),expand = c(0,0))

#5.Histogram for snowmelt
snow <- pop_expo_flood_type |> 
  filter(flood_cat == "Snowmelt")

quants <- quantile(snow$perc_pop_flood_2015, probs = c(0.25,0.5,0.75))  

snow_hist <- snow %>% 
  ggplot(aes(perc_pop_flood_2015)) + geom_histogram(binwidth = 0.0001, col = "black", fill = "grey") + 
  geom_vline(xintercept = quants[1], color = "green3", size = 1.0) + 
  geom_vline(xintercept = quants[2], color = "blue", size = 1.0) +
  geom_vline(xintercept = quants[3], color = "red", size = 1.0) + 
  theme_bw() + theme(text = element_text(size = 16),
                     legend.text=element_text(size=12), legend.title = element_text(size = 12),
                     panel.grid.major = element_blank(),axis.text.x = element_text(size = 12, angle= 0), axis.text.y = element_text(size=10),
                     plot.margin=grid::unit(c(0.5,0.5,0.5,0.5), "cm"), 
                     plot.title = element_text(hjust = 0.5), panel.background = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
                     legend.position = 'bottom',legend.justification='center',
                     legend.background = element_rect(fill="white", size=.5, linetype="dotted")) +
  xlab('') + ylab('') +
  scale_y_continuous(expand = c(0,0), limits = c(0,50)) +
  scale_x_continuous(limits = c(0,0.3),expand = c(0,0))

#6.Histogram for ice jam or dam break
ice <- pop_expo_flood_type |> 
  filter(flood_cat == "Ice jam or dam break")

quants <- quantile(ice$perc_pop_flood_2015, probs = c(0.25,0.5,0.75))  

ice_hist <- ice %>% 
  ggplot(aes(perc_pop_flood_2015)) + geom_histogram(binwidth = 0.0001, col = "black", fill = "grey") + 
  geom_vline(xintercept = quants[1], color = "green3", size = 1.0) + 
  geom_vline(xintercept = quants[2], color = "blue", size = 1.0) +
  geom_vline(xintercept = quants[3], color = "red", size = 1.0) + 
  theme_bw() + theme(text = element_text(size = 16),
                     legend.text=element_text(size=12), legend.title = element_text(size = 12),
                     panel.grid.major = element_blank(),axis.text.x = element_text(size = 12, angle= 0), axis.text.y = element_text(size=10),
                     plot.margin=grid::unit(c(0.5,0.5,0.5,0.5), "cm"), 
                     plot.title = element_text(hjust = 0.5), panel.background = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
                     legend.position = 'bottom',legend.justification='center',
                     legend.background = element_rect(fill="white", size=.5, linetype="dotted")) +
  xlab('') + ylab('') +
  scale_y_continuous(expand = c(0,0), limits = c(0,50)) + 
  scale_x_continuous(limits = c(0,0.2),expand = c(0,0))


#4b.Save median max duration 2x2 plots 

hist_plot <- ggarrange(overall_hist, rain_hist, tcs_hist, snow_hist, ice_hist,
          ncol = 3, nrow = 2,
          vjust = 0.1,
          hjust = c(-0.4, -0.4, -0.3, -0.4, -0.2),
          labels = c("a) All floods", "b) Heavy rain", "c) Tropical cyclone", "d) Snowmelt", "e) Ice jam or dam break"),
          font.label = list(face = 'plain'),
          common.legend = FALSE) +
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))

hist_final <- annotate_figure(hist_plot,left = textGrob("Count", rot = 90, vjust = 2, gp = gpar(cex = 1.3)),
                         bottom = textGrob("Percent of county population in flooded area", vjust = -2,gp = gpar(cex = 1.3)))

jpeg(paste0(figures.folder, 'Figure S1.jpeg'), res = 300, height = 2000, width = 3000)
hist_final
dev.off()

