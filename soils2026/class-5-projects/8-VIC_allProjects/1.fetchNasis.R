library(aqp)
library(soilDB)
library(sf)


spc <- fetchNASIS()

site <- site(spc)
names(site)

site.df <- site[c(11,17,18, 81, 85, 86,90,93,94,139,140,143,144)]

site.df <- site.df[complete.cases(site.df$x_std),]
site.df <- site.df[complete.cases(site.df$y_std),]

site.pts <- st_as_sf(site.df, 
                     coords = c('x_std','y_std'),
                     crs = st_crs(4326))
setwd("C:/8-VIC-class5Projects")


st_write(site.pts, "All_pedons.shp")
