#objects

#years included in analysis
start_year = 2001
end_year = 2018
years_analysis = c(start_year:end_year)

#flood categories
storm_types <- c("Tropical storms", "Extra-tropical cyclone", "Monsoon rain", "Hurricane", "Storm surge")
rain_types <- c("Heavy rain", "Brief torrential rain", "Heavy rain and snowmelt")
snow_types <- c("Snowmelt", "Heavy rain and snowmelt")
break_types <- c("Dam failure", 'Ice jams')

#US map data (from Jonathan Sullivan's github)
theme_map <- function(base_size=15, base_family=""){
  require(grid)
  theme_bw(base_size=base_size,base_family=base_family) %+replace%
    theme(axis.line=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid=element_blank(),
          panel.margin=unit(0,"lines"),
          plot.background=element_blank(),
          legend.position = 'bottom'
    )
}


#flood colors
all.floods.map = c('cornsilk', met.brewer(name='Tam')) 
heavy.rain.floods.map = c('cornsilk', met.brewer(name='Hokusai3')) 
storms.floods.map = c('cornsilk', met.brewer(name='OKeeffe2')) 
rain.snow.floods.map = c('cornsilk', met.brewer(name='VanGogh3')) 
ice.jam.floods.map = c('cornsilk', met.brewer(name='Hokusai2'))

#other colors
age.colors = c('grey','cornsilk', met.brewer(name = 'Isfahan1'))

