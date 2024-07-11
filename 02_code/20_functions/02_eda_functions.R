#1.Function to plot flood maps 
map_floods_function <- function(map_variables, flood_cats){
  
  for(map_var in map_variables){
    print(map_var)
    
    for(fc in flood_cats){
      print(fc)
      
      map_data <- USA.df.summary.flood |> filter(map_var_type == map_var & flood_cat == fc) |> 
        left_join(map_settings, by = c("flood_cat"="flood_cats", 'map_var_type' = "map_variables")) 
      
      plot_label <- paste0(map_data$label[1])
      plot_color_char <- paste0(map_data$color[1])
      plot_color <- eval(as.symbol(plot_color_char))
      
      max <- max(map_data$map_var_value)
      
      plot = ggplot() +
        
        geom_polygon(data=states,aes(x=x, y=y,group=group), fill = 'cornsilk') + 
        
        geom_polygon(data=map_data,aes(x=long,y=lat, group=group,fill=map_var_value)) + #size = 0.001
        
        geom_polygon(data=states,aes(x=x, y=y,group=group), color = "lightgray", fill = alpha(0.01), size = 0.2) + 
        
        guides(fill = guide_colorbar(direction = "horizontal", title.position="left",barwidth = 8, 
                                     barheight = 1,title.vjust = 0.8, title = paste0(fc,' flood\n',plot_label),legend.text=element_text(size=8))) +
        scale_fill_gradientn(colors = plot_color, limits = c(0,max), labels =function(x) paste0(format(round(x,0)))) + 
        
        coord_fixed() + xlab('') + ylab('') + theme_bw() +
        theme(panel.grid.major = element_blank(),
              axis.text.x = element_text(angle=90), axis.ticks.x=element_blank(),legend.text=element_text(size=20),
              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
              panel.border = element_rect(colour = "black"),strip.background = element_blank(),legend.justification='center',
              legend.position = 'bottom', legend.background = element_rect(fill="white", size=.5, linetype="dotted")) +
        theme_map() 
      
      #jpeg(paste0(figures.folder,fc,'_',map_var,'_map.jpeg'), res = 400, height = 3000, width = 3500)  #use this if you want to separately save the map plots 
      #print(plot)
      #dev.off()
      
    }
  }  
  return(plot)
}
