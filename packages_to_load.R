#1.Load packages on CRAN

#1a.Add new packages here, as necessary 
list.of.packages = c('acs','BiocManager','dlnm','dplyr','ecm','Epi','fiftystater','foreign', 'fst','ggpubr','ggplot2', 'graph','graticule',
                     'haven','here', 'janitor','lubridate', 'mapproj','maptools','mapview','MetBrewer','pipeR','raster',
                     'RColorBrewer','readxl', 'rgdal', 'rgeos','rnaturalearth','rnaturalearthdata','scales', 'sf','sp','sqldf', 'survival','splines',
                     'table1', 'tidycensus', 'tidyverse', 'totalcensus', 'usmap','zipcodeR','zoo', 'INLA', 'Rgraphviz','fmesher')

#1b.Check if list of packages is installed. If not, it will install ones not yet installed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) invisible(install.packages(new.packages,repos = "https://cloud.r-project.org"))

#1c.load packages
invisible(lapply(list.of.packages, require, character.only = TRUE, quietly=TRUE))

#devtools::install_github("wmurphyrd/fiftystater")
