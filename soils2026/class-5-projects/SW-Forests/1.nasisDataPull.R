#1.nasisDataPull.R 
#workflow for gathering pedon data from nasis to create esri shapefiles
# 05/30/24
# dave white


# nasis data pull for pedons owned by offices

# run query: 'Pedons and Sites by Group +'
# to load pedons into selected set owned by offices
# run each of the following nasis groups

#SW-FLA Data Group
#SW-GLO Data Group
#SW-GRA Data Group
#SW-LAS Data Group
#SW-MAF Data Group
#SW-RIC Data Group
#SW-SAN Data Group
#SW-TUC Data Group
#________________________________


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
st_write(site.pts, "pedFS.shp")











