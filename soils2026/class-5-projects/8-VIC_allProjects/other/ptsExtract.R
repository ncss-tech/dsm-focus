# 8-VIC 30m covaiate extract 03/13/24

# load and install package"s
required.packages <- c("sf", "terra")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)


# set working directory to dem covs
setwd("~/data/8-vic/cov30")
# read in raster file names as a list
rList=list.files(getwd(), pattern="tif$", full.names = FALSE)
# create a raster stack of covs
demStack <- rast(rList)

# set working directory to spectral covs
setwd("~/data/8-vic/specCov30/clip")
# read in raster file names as a list
rList=list.files(getwd(), pattern="tif$", full.names = FALSE)
# create a raster stack of covs
specStack <- rast(rList)

# combine into one stack
rStack <- c(demStack,specStack)


# Bring in training data points 
# read in shapefile 
# set working directory to points
setwd("~/data/8-vic/data")
getwd()

# change the name to match your shapefile

# all data no splitting
all.pts <- st_read("pedons.shp")


# remove unwanted cols
names(all.pts)
all.pts <- all.pts[-c(16,17)]


# extract raster covariate values at each training point for the all.pts dataset
pts.sv <- terra::extract(rStack, all.pts, xy=F, bind=TRUE, na.rm=TRUE) 

# convert SpatVector to sf 
all.pts <- sf::st_as_sf(pts.sv) 

setwd("~/data/8-vic/data")
# save rds
saveRDS(all.pts, file="pedonsExtract.RDS")

# load rds
test <- readRDS("pedonsExtract.RDS")

