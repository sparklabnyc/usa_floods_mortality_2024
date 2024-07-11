plot_all_flood_severity <- function(plot_group, expo_type){
  
  for(types in expo_types){
    print(types)
    
    for (plot_groups in plot_group){
      print(plot_groups)
    
    plot_data <- tidy_plot_data %>% 
      filter(type == expo_types & plot_group == plot_groups) %>% 
      rename(lower_ci = '0.025quant',
             upper_ci = '0.975quant') %>% 
      mutate(threshold = case_when(threshold == '1_pert' ~ 'a1_pert', 
                                   threshold == '25_pert' ~ 'b25_pert', 
                                   threshold == '50_pert' ~ 'c50_pert', 
                                   threshold == '75_pert' ~ 'd75_pert'))
    
    if(plot_groups == "overall_non_spf"){
      
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
        scale_y_continuous(labels=scales::percent_format(accuracy=0.1)) +
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
        facet_wrap(~cause, scales = "free_y") +
        theme(panel.spacing = unit(0.6, "cm"))
      
      jpeg(paste0(figures.folder,types,'_all_severity_non_flood_spf.jpeg'), res = 400, height = 3000, width = 3500)
      print(plot_overall)
      dev.off()
    }
    
    if(plot_groups == "overall"){
      
      plot_overall <-  ggplot(data=plot_data) +
        geom_errorbar(data = filter(plot_data, threshold == "a1_pert"),
                      aes(x=rowname, ymax=upper_ci,ymin=lower_ci, color = threshold), size = 0.5, width = 0, color = "black",
                      position = position_nudge(x = -0.225)) +
        geom_point(data = filter(plot_data, threshold == "a1_pert"), 
                   aes(x=rowname,y=mean, color = threshold), size = 2.5, position = position_nudge(x = -0.225)) +
        
        
        geom_errorbar(data = filter(plot_data, threshold == "b25_pert"),
                      aes(x=rowname, ymax=upper_ci,ymin=lower_ci, color = threshold), size = 0.5, width = 0, color = "black",
                      position = position_nudge(x = -0.075)) +
        geom_point(data = filter(plot_data, threshold == "b25_pert"), 
                   aes(x=rowname,y=mean, color = threshold), size = 2.5, position = position_nudge(x = -0.075)) +
        
        
        geom_errorbar(data = filter(plot_data, threshold == "c50_pert"),
                      aes(x=rowname, ymax=upper_ci,ymin=lower_ci, color = threshold), size = 0.5, width = 0, color = "black",
                      position = position_nudge(x = 0.075)) +
        geom_point(data = filter(plot_data, threshold == "c50_pert"), 
                   aes(x=rowname,y=mean, color = threshold), size = 2.5, position = position_nudge(x = 0.075)) +
        
        geom_errorbar(data = filter(plot_data, threshold == "d75_pert"),
                      aes(x=rowname, ymax=upper_ci,ymin=lower_ci, color = threshold), size = 0.5, width = 0, color = "black",
                      position = position_nudge(x = 0.225)) +
        geom_point(data = filter(plot_data, threshold == "d75_pert"), 
                   aes(x=rowname,y=mean, color = threshold), size = 2.5, position = position_nudge(x = 0.225)) +
        
        geom_hline(yintercept=0,linetype='dotted', size = 0.5) +
        xlab('Month after flood event') + ylab('Percent change in death rates') +
        scale_color_manual(values=c("#4EB3D3","#2B8CBE", "#0868AC", "#084081"), guide = guide_legend(nrow = 1,title = paste0("Flood severity")),
                           labels = c("Mild","Moderate","Severe", "Very severe")) +
        scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
        scale_shape_manual(values=seq(0,15)) +
        labs(colour="", shape="") +
        theme_bw() + theme(text = element_text(size = 12),
                           legend.text=element_text(size=10), legend.title = element_text(size = 10),
                           panel.grid.major = element_blank(),axis.text.x = element_text(size = 10, angle= 0), axis.text.y = element_text(size=10),
                           plot.margin=grid::unit(c(0.5,0.5,0.5,0.5), "mm"), 
                           plot.title = element_text(hjust = 0.5), panel.background = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                           panel.border = element_rect(colour = "black"),strip.background = element_blank(),
                           legend.position = 'bottom',legend.justification='center',
                           legend.background = element_rect(fill="white", size=.5, linetype="dotted")) +
        facet_grid(flood_cat~cause, 
                   scales = "free_y", labeller = labeller(flood_cat = floodtypes.labs, cause = causes.labs)) +
        theme(panel.spacing = unit(0.6, "cm"))
      

      plot_flood_spf <- plot_overall +
        ggh4x::facetted_pos_scales(
          y = list(
            scale_y_continuous(limits = c(-0.10, 0.08), labels=scales::percent_format(accuracy=1)),
            scale_y_continuous(limits = c(-0.20,0.14), labels=scales::percent_format(accuracy=1)),
            scale_y_continuous(limits = c(-0.10,0.25), labels=scales::percent_format(accuracy=1)),
            scale_y_continuous(limits = c(-0.10,0.25), labels=scales::percent_format(accuracy=1))
          )
        )

      jpeg(paste0(figures.folder,types,'_all_severity_flood_spf.jpeg'), res = 400, height = 3000, width = 5000)
      print(plot_flood_spf)
      dev.off()
    }
    
    if(plot_groups == "sex"){
      
      plot_sex <-  ggplot(data=plot_data) +
        
        geom_errorbar(data = filter(plot_data, threshold == "d75_pert" & group == "1"),
                      aes(x=rowname, ymax=upper_ci,ymin=lower_ci, color = threshold), size = 0.5, width = 0, color = "black",
                      position = position_nudge(x = 0.15)) +
        geom_point(data = filter(plot_data, threshold == "d75_pert" & group == "1"), 
                   aes(x=rowname,y=mean, color = group, shape = threshold), size = 3, position = position_nudge(x = 0.15)) +
        
        geom_errorbar(data = filter(plot_data, threshold == "d75_pert" & group == "2"),
                      aes(x=rowname, ymax=upper_ci,ymin=lower_ci, color = threshold), size = 0.5, width = 0, color = "black",
                      position = position_nudge(x = -0.15)) +
        geom_point(data = filter(plot_data, threshold == "d75_pert" & group == "2"), 
                   aes(x=rowname,y=mean, color = group, shape = threshold), size = 3, position = position_nudge(x = -0.15)) +
        
        geom_hline(yintercept=0,linetype='dotted') +
        xlab('Month after flood event') + ylab('Percent change in death rates') +
        guides(shape = "none") +
        scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
        scale_color_manual(values = c("darkgreen", "orange"), guide = guide_legend(nrow = 1,title = paste0("Sex")), labels = c("Male", "Female")) +
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
        facet_grid(flood_cat~cause,labeller = labeller(flood_cat = floodtypes.labs, cause = causes.labs), scales = "free_y") +
        theme(panel.spacing = unit(0.6, "cm"))       
      
      plot_flood_spf_sex <- plot_sex +
        ggh4x::facetted_pos_scales(
          y = list(
            scale_y_continuous(limits = c(-0.08,0.13), labels=scales::percent_format(accuracy=1)),
            scale_y_continuous(limits = c(-0.20,0.13), labels=scales::percent_format(accuracy=1)),
            scale_y_continuous(limits = c(-0.10,0.25), labels=scales::percent_format(accuracy=1)),
            scale_y_continuous(limits = c(-0.10,0.27), labels=scales::percent_format(accuracy=1))
          )
        )
      
      jpeg(paste0(figures.folder,types,'_very_severe_flood_type_sex.jpeg'), res = 400, height = 3000, width = 5000)
      print(plot_flood_spf_sex)
      dev.off()
    }
    
    if(plot_groups == "age"){
      
      plot_age <-  ggplot(data=plot_data) +
        
        geom_errorbar(data = filter(plot_data, threshold == "d75_pert" & group == "64"),
                      aes(x=rowname, ymax=upper_ci,ymin=lower_ci, color = threshold), size = 0.5, width = 0, color = "black",
                      position = position_nudge(x = 0.15)) +
        geom_point(data = filter(plot_data, threshold == "d75_pert" & group == "64"), 
                   aes(x=rowname,y=mean, color = group, shape = threshold), size = 3, position = position_nudge(x = 0.15)) +
        
        geom_errorbar(data = filter(plot_data, threshold == "d75_pert" & group == "66"),
                      aes(x=rowname, ymax=upper_ci,ymin=lower_ci, color = threshold), size = 0.5, width = 0, color = "black",
                      position = position_nudge(x = -0.15)) +
        geom_point(data = filter(plot_data, threshold == "d75_pert" & group == "66"), 
                   aes(x=rowname,y=mean, color = group, shape = threshold), size = 3, position = position_nudge(x = -0.15)) +
        
        geom_hline(yintercept=0,linetype='dotted') +
        xlab('Month after flood event') + ylab('Percent change in death rates') +
        guides(shape = "none") +
        scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
        scale_color_manual(values = c("magenta4", "aquamarine3"), guide = guide_legend(nrow = 1,title = paste0("Age")), labels = c("0-64 years", "≥65 years")) +
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
        facet_grid(flood_cat~cause,labeller = labeller(flood_cat = floodtypes.labs, cause = causes.labs), scales = "free_y") +
        theme(panel.spacing = unit(0.6, "cm"))         
      
      plot_flood_spf_age <- plot_age +
        ggh4x::facetted_pos_scales(
          y = list(
            scale_y_continuous(limits = c(-0.08,0.10), labels=scales::percent_format(accuracy=1)),
            scale_y_continuous(limits = c(-0.23,0.10), labels=scales::percent_format(accuracy=1)),
            scale_y_continuous(limits = c(-0.16,0.25), labels=scales::percent_format(accuracy=1)),
            scale_y_continuous(limits = c(-0.15,0.), labels=scales::percent_format(accuracy=1))
          )
        )
      
      jpeg(paste0(figures.folder,types,'_very_severe_flood_type_age.jpeg'), res = 400, height = 3000, width = 5000)
      print(plot_flood_spf_age)
      dev.off()
    }
    }
  }
}

