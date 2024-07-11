#load shapefile of entire United States by county
us.national <- readOGR(dsn=paste0(support.data.folder,"cb_2015_us_county_500k"), layer = "cb_2015_us_county_500k")
#reproject shapefile
us.national = spTransform(us.national, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
#remove non-mainland territories (assuming it's for entire mainland US)
us.main = us.national[!us.national$STATEFP %in% c("02","15","60","66","69","71","72","78"),]
#fortify to prepare for plotting in ggplot
map = fortify(us.main)
#extract data from shapefile
us.main@data$id = rownames(us.main@data)
shapefile.data = us.main@data
#merge selected data to map_create dataframe for colouring of ggplot
USA.df = merge(map, shapefile.data, by='id')
USA.df$GEOID = as.integer(as.character(USA.df$GEOID))