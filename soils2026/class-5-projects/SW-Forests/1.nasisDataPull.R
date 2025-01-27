# R script to generate shapefile of pedons owned by offices

# Load Libraries
library(aqp)
library(soilDB)
library(sf)

# load soils data into soil profile collection
spc <- fetchNASIS()

# get the site table from the soil profile collection and name it site
site.df <- site(spc)
# inspect the attribute names
names(site)


site.df <- site.df[complete.cases(site.df$x_std),]
site.df <- site.df[complete.cases(site.df$y_std),]

site.pts <- st_as_sf(site.df, 
                     coords = c('x_std','y_std'),
                     crs = st_crs(4326))

setwd("D:/soils_2026/S2026class5projects/SW2026USFSlands/pedons")
st_write(site.pts, "pedOff.shp")

# when making esri shapefile the attribute names get shortened
# get original names and abbreviations and export as table for reference

# get the original col names
siteNames <- names(site.df)

# get the abbreviated col names
siteAbbrev <- names(sf:::abbreviate_shapefile_names(site.df)) #function embeded within st_write, used when writing to esri shapefile

# combine names into dataframe
siteNames.df <- as.data.frame(cbind(siteNames, siteAbbrev))

# write table
write.csv(siteNames.df, file="pedOffAb")


########

#1.nasisDataPull.R 
#workflow for gathering  USFS pedon data from nasis to create esri shapefiles
# 05/30/24
# dave white


# nasis data pull for FS pedons

# run query: '40MIS USFS NRM PEDONS by national forest area'
# to load pedons into selected set
# run each of the following 

#RO3% 
# this is the southwest forest region

#________________________________


# R script to generate shapefile of pedons owned by offices

# Load Libraries
library(aqp)
library(soilDB)
library(sf)

# load soils data into soil profile collection
spc <- fetchNASIS(fill=T)

# get the site table from the soil profile collection and name it site
site.df <- site(spc)

# removing any observations missing the standard x y cols from NASIS
site.df <- site.df[complete.cases(site.df$x_std),]
site.df <- site.df[complete.cases(site.df$y_std),]

# remove any ovservations missing the taxonomic classification
side.df <- site.df[complete.cases(site.df$taxclname),]

# inspect the attribute names
names(site.df)
names(site.df)[c(1,2,103,174,175)]
head(site.df)[c(1,2,103,174,175)]

site.df <- (site.df)[c(1,2,17,18,103,174,175)]

site.pts <- st_as_sf(site.df, 
                     coords = c('x_std','y_std'),
                     crs = st_crs(4326))

# load buffered project boundary
bndry <- st_read("D:/soils_2026/class-5-projects/SW2026USFSlands/s2026-class5-sw-forests/data/projectBoundary/bndry-2kbuf.shp")

# project site.pts to match bndry
site.pts <- st_transform(site.pts, crs = st_crs(bndry))

#clip the points to the bndry
site.pts.clip <- st_intersection(site.pts, bndry)

plot(site.pts.clip)

setwd("D:/soils_2026/class-5-projects/SW2026USFSlands/s2026-class5-sw-forests/data/nasis-export")
st_write(site.pts.clip, "nasis-exp-127.shp", delete_layer = T)
